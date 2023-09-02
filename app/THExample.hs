module THExample where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Lib
import Control.Monad (unless)
import Types
import Data.Data (Proxy(Proxy))
import Data.Generics

add1 :: Quote m => Int -> m Exp
add1 x = [| x + 1 |]

data Format = D | S | L String

parse :: String -> [Format]
parse s   = [ L s ]

gen :: Quote m => [Format] -> m Exp
gen [D]   = [| \n -> show n |]
gen [S]   = [| \s -> s |]
gen [L s] = stringE s

pr :: Quote m => String -> m Exp
pr s = gen (parse s)

-- \a -> a + 4
add4 :: Q Exp
add4 = do
  x <- newName "x"
  return $ LamE [VarP x] (InfixE (Just (VarE x)) (VarE '(+)) (Just (LitE (IntegerL 4))))

recordWildCard :: Q Exp
recordWildCard = do
  x <- newName "x"
  return $ LamE [VarP x] (InfixE (Just (VarE x)) (VarE '(+)) (Just (LitE (IntegerL 4))))

add4' :: Q Exp
add4' = [e|\x -> x + 4|]


lambdaXplus1 :: Q Exp
lambdaXplus1 = do
  x <- newName "x" -- unique variable name
  return $ LamE    -- lambda expression
    [VarP x]       -- pattern matching on 'x'
    (InfixE (Just (VarE x)) (VarE '(+)) (Just (LitE (IntegerL 1))))

genPrintFun :: Name -> Name -> DecQ
genPrintFun dType dFiltertype = do
  let printFunBody = NormalB (AppE (VarE $ mkName "return") (LitE $ StringL $ "Dynamic Lens " <>  nameBase dType <> " " <> nameBase dFiltertype))
      printFun = FunD (mkName "printFiltertype") [Clause [WildP, WildP] printFunBody []]
  return printFun



deriveCountableSimple :: Name -> Q [Dec]
deriveCountableSimple name = [d|
  instance Countable $a where
    count Proxy = fromIntegral $
      1 + fromEnum (maxBound :: $a) - fromEnum (minBound :: $a)
  |]
  where
    a = conT name

deriveCountableComposite :: Name -> Q [Dec]
deriveCountableComposite name = do
  TyConI (DataD _ _ _ _ cons' _) <- reify name
  [d|
     instance Countable $(conT name) where
       count Proxy = $(foldr addE [| 0 |] $ f <$> cons')
   |]
  where
    f (NormalC _ ts) = handleCon (snd <$> ts)
    f (RecC    _ ts) = handleCon (thd <$> ts)
    -- f (NormalC n _) = handleCon n
    -- f (RecC    n _) = handleCon n
    f _              = fail "unsupported data type"
    handleCon ts = foldr mulE [| 1 |] (countTypeE <$> ts)
    -- handleCon ts = [| 1 |]
    -- countTypeE t = [| count (Proxy :: Proxy $(return t)) |]
    countTypeE t = [| "" |]
    addE x y     = [| $x + $y |]
    mulE x y     = [| $x * $y |]
    thd (_,_,x)  = x

-- >>> runQ (reify ''Bool)
-- user error (Template Haskell failure)


deriveToString :: Name -> Q [Dec]
deriveToString name = do
  TyConI (DataD _ _ _ _ cons' _) <- reify name
  [d|
    instance ToString $(conT name) where
      toString Proxy = $(foldr addE [| "" |] $ f <$> cons')
   |]

  where
    f (NormalC n _) = handleCon n
    f (RecC    n _) = handleCon n
    f _              = fail "unsupported data type"
    handleCon  _ = [| " " |]
    -- handleCon n = return $ LitE $ StringL $ nameBase n

    -- handleCon n = foldr mulE [| "" |] (stringifyE n)
    -- stringifyE n = return $ LitE $ StringL $ nameBase n
    -- stringifyE n = [| "" |]
    addE x y = [| $x <> " + " <> $y |]
    mulE x y = [| $x <> " * " <> $y |]

-- deriveToStringHelper :: Name -> Q [(Name, [(Name, Type)])]
-- deriveToStringHelper name = do
--   info <- reifyDatatype name
--   let consL = datatypeCons info
--       nConsL = mapM normalizeConstructor consL
--   -- cons <- case consL of
--   --   [cons'] -> return cons'
--   --   [] -> fail $ "No constructor found for " <> nameBase name
--   --   _multipleCons -> fail $ "Multiple constructr found for " <> nameBase name
--   -- normalizeConstructor cons
--   mapM normalizeConstructor consL

normalizeConstructor :: ConstructorInfo -> Q (Name, [(Name, Type)])
normalizeConstructor con = do
  fieldNames <- case constructorVariant con of
    RecordConstructor xs -> return xs
    _otherCons -> fail $ show _otherCons <> " found for " <> nameBase (constructorName con)
  return (constructorName con, zip fieldNames (constructorFields con))

handleToStringHelper :: Name -> Q Exp
handleToStringHelper name = do
  info <- reifyDatatype name
  let consL = datatypeCons info
  allCons <- mapM normalizeConstructor consL
  let allNames' = map stringifyHelper allCons
      allNames = show allNames'
  return $ LitE $ StringL allNames
  where
    stringifyHelper (pname, snames) = do
      let allNames' = map (\(n, _) -> nameBase n) snames
          allNames = nameBase pname : allNames'
      allNames

handleToShowValueHelper :: Name -> Q Exp
handleToShowValueHelper name = do
  let x = mkName "x"
  let c = mkName "c"
  let d = mkName "d"
  let s = mkName "show"
  let v = mkName "V"
  let ab = mkName "AB"
  return $ CaseE (VarE x)
    [
      Match
        (RecP v [])
        (NormalB (AppE (VarE s) (AppE (VarE c) (VarE x))))
        [],
      Match
        WildP
        (NormalB (AppE (VarE s) (AppE (VarE d) (VarE x))))
        []
    ]

deriveToString'' :: Name -> Q [Dec]
deriveToString'' name = do
  [d|
    instance ToString $(conT name) where
      toString Proxy = $(handleToString name)
      showValue x = $(handleShowValue name) -- to do this, i need to get a lambda or a function, with case match
   |]
  where
    handleToString = handleToStringHelper
    handleShowValue = handleToShowValueHelper


addMaybesAndOpts :: Maybe String -> Dec -> Q Dec
addMaybesAndOpts modName dec =
  -- Apply @rename@ and @addMaybe@ everywhere in the
  -- declaration @dec@.
  --
  -- The SYB, @everywhere (mkT (f :: a -> a)) (x :: b)@
  -- applies @f@ to all data of type @a@ in @x@, and
  -- @everywhereM (mkM (f :: a -> m a) (x :: b)@ is
  -- similar, but applies @f@ everywhere in @x@ monadically.
  everywhere (mkT rename) <$>
  everywhereM (mkM addMaybe) dec
  where
    -- Add the "_opt" suffix to a name, if it's from
    -- the given module.
    rename :: Name -> Name
    rename n = if nameModule n == modName
        then mkName $ nameBase n ++ "_opt"
        else n
 
    -- Wrap the type of a record field in @Maybe@.
    addMaybe :: (Name, Strict, Type) -> Q (Name, Strict, Type)
    addMaybe (n, s, ty) = do
      ty' <- [t| Maybe $(return ty) |]
      return (n,s,ty')
 
mkOptional :: Name -> Q Dec
mkOptional n = do
    TyConI dec <- reify n
    addMaybesAndOpts (nameModule n) dec

-- mkToStrings :: Name -> Q Exp
-- mkToStrings n = do
--   TyConI dec <- reify n
--   getToStrings dec
--
-- getToStrings :: Dec -> Q Exp
-- -- getToStrings dec = [| "hello" |]
-- -- getToStrings dec = LitE $ StringL $ "Dynamic Lens " <>  nameBase dType <> " " <> nameBase dSubtype
-- getToStrings dec = do
--   (mkT toString) dec
--   where
--     toString n = return $ LitE $ StringL $ "hello there" <> nameBase n


-- generateCode :: Name -> Q [Dec]
-- generateCode typeName = do
--     info <- reify typeName
--     case info of
--         TyConI (DataD _ _ _ _ constructors _) ->
--             let generatedClauses = map generateClause constructors
--                 showInstance = InstanceD Nothing [] (AppT (ConT ''Show) (ConT typeName)) [FunD 'show generatedClauses]
--             in return [showInstance]
--         _ -> fail "Invalid input"

-- generateClause :: Con -> Clause
-- generateClause = do
--   let
--   undefined

-- generateClause :: Con -> Clause
-- generateClause (NormalC conName fields) =
--     let conPattern = ConP conName (map (\(fieldName, _) -> VarP fieldName) fields)
--         showExpr = AppE (VarE 'show) (TupE (map (\(fieldName, _) -> VarE fieldName) fields))
--         body = NormalB $ AppE (VarE 'concat) [LitE (StringL (nameBase conName ++ " {")), showExpr, LitE (StringL "}")]
--     in Clause [conPattern] body []
-- generateClause _ = error "Unsupported constructor type"

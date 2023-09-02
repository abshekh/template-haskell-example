{-# LANGUAGE TupleSections #-}

module FilterByTH (deriveFilterByClass) where

import Control.Arrow (Arrow (second))
import Control.Monad (unless)
import Data.Data (Proxy (Proxy))
import Data.Generics
import FilterByClass
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import GHC.Exts (IsList(fromList))

normalizeConstructor :: ConstructorInfo -> Q (Name, [(Name, Type)])
normalizeConstructor con = do
  fieldNames <- case constructorVariant con of
    RecordConstructor xs -> return xs
    _otherCons -> fail $ show _otherCons <> " found for " <> nameBase (constructorName con)
  return (constructorName con, zip fieldNames (constructorFields con))

getTypeInfo :: Name -> Q [(Name, [(Name, Type)])]
getTypeInfo dType = do
  info <- reifyDatatype dType
  let consL = datatypeCons info
  mapM normalizeConstructor consL

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

handleGetKey :: Name -> Q Exp
handleGetKey filterByName = do
  let filterBy = mkName "filterBy"
      showE = mkName "show"
  filterTypeInfo <- getTypeInfo filterByName
  matches <- mapM (getMatches showE) filterTypeInfo
  return $
    CaseE
      (VarE filterBy)
      matches

getMatches showE (pname, cnames) = do
  wildCNames <- mapM getWildNames cnames
  let wildCPatterns = map (second VarP) wildCNames
  let pattern = RecP pname wildCPatterns
  let body = NormalB $ UInfixE (LitE (StringL $ "" <> nameBase pname)) (VarE $ mkName "++") (showWildNames showE wildCNames)
  return $ Match pattern body []

getWildNames (name, _) = do
  rName <- newName (nameBase name)
  return (name, rName)

showWildNames showE wildCNames = do
  let lShowWildNames = map (\(_, n) -> AppE (VarE showE) (VarE n)) wildCNames
  foldr
    (\curr prev -> UInfixE (UInfixE (LitE $ StringL "-") (VarE $ mkName "++") curr) (VarE $ mkName "++") prev)
    (LitE (StringL ""))
    lShowWildNames


handleGetAllKeys' :: Name -> Name -> Q Exp
handleGetAllKeys' tableRecordName filterByName = do
  let tableRecord = mkName "tableRecord"
      showE = mkName "show"
  tableRecordInfo <- getTypeInfo tableRecordName
  filterTypeInfo <- getTypeInfo filterByName
  ListE <$> step1 tableRecord tableRecordInfo filterTypeInfo showE
  -- return $ LitE $ StringL $ show tableRecordInfo ++ " <-> " ++ show filterTypeInfo
  -- matches <- mapM (getMatches' showE) tableRecordInfo
  -- return $
  --   CaseE
  --     (VarE tableRecord)
  --     matches
  where
    hashmap _ [] = fail "no cons in tablerecord"
    hashmap a [x] = do
      let (name, names) = x
      lookup a names
    hashmap _ (x:xs) = fail "too many cons in tablerecord"
    step1 tableRecord tableRecordInfo filterTypeInfos showE = do
      mapM (\f -> auxStep1 tableRecord tableRecordInfo f showE) filterTypeInfos
    auxStep1 tableRecord tableRecordInfo filterTypeInfo showE = do
      let (fname, rest') = filterTypeInfo
      let [(name', _)] = tableRecordInfo
      matches <- mapM (\ t -> getMatches' showE t fname) [(name', rest')]
      -- matches <- mapM (\ t -> getMatches' showE t fname) tableRecordInfo
      return $
        CaseE
          (VarE tableRecord)
          matches

getMatches' showE (pname, cnames) fname = do
  wildCNames <- mapM getWildNames cnames
  let wildCPatterns = map (second VarP) wildCNames
  let pattern = RecP pname wildCPatterns
  let body = NormalB $ UInfixE (LitE (StringL $ "" <> nameBase fname)) (VarE $ mkName "++") (showWildNames' showE wildCNames)
  return $ Match pattern body []

showWildNames' showE wildCNames = do
  let lShowWildNames = map (\(_, n) -> AppE (VarE showE) (VarE n)) wildCNames
  foldr
    (\curr prev -> UInfixE (UInfixE (LitE $ StringL "-") (VarE $ mkName "++") curr) (VarE $ mkName "++") prev)
    (LitE (StringL ""))
    lShowWildNames


handleGetAllKeys :: Name -> Name -> Q Exp
handleGetAllKeys tableRecordName filterByName = do
  let tableRecord = mkName "tableRecord"
      showE = mkName "show"
  tableRecordInfo <- getTypeInfo tableRecordName
  filterTypeInfo <- getTypeInfo filterByName
  ListE <$> getAllFilterBy showE tableRecordInfo filterTypeInfo

-- getValueFromRecord :: Name -> Name -> [(Name, [(Name, Type)])] -> Q (Maybe Exp)
getValueFromRecord :: Name -> Name -> [(Name, [(Name, Type)])] -> Q Exp
getValueFromRecord _ _ [] = fail "No cons in Table Record"
getValueFromRecord showE fname [(pname, cnames)] = do
  wildCNames <- mapM getWildNames cnames
  let wildCNames' = map (\(n, wn) -> (mkName (nameBase n), wn)) wildCNames
  let wildCPatterns = map (second VarP) wildCNames
  let pattern = [RecP pname wildCPatterns]
  let exp = case findWildCName (mkName (nameBase fname)) wildCNames' of
                -- Just n -> Just $ AppE (VarE showE) (VarE n)
                Just n -> UInfixE (LitE $ StringL "-prev-") (VarE $ mkName "++") $ AppE (VarE showE) (VarE n)
                -- _ -> fail "exp"
                _ -> LitE (StringL $ "" ++ nameBase fname ++ " <-> " ++ show (map show wildCPatterns) ++ " <-> " ++ show (map show wildCPatterns))
                -- _ -> Nothing
  -- return $ LamE pattern <$> exp
  return exp
getValueFromRecord _ _ (x : [xs]) = fail "More than one cons found in Table Record"

findWildCName fname x@[(a, b)] = lookup fname $ fromList x
-- findWildCName _ _ = Nothing
findWildCName _ _ = fail "findWildCName"

getAllFilterBy :: Name ->  [(Name, [(Name, Type)])]  ->  [(Name, [(Name, Type)])] -> Q [Exp]
getAllFilterBy showE tableRecordInfo filterTypeInfo = do
  allFilters' <- mapM (\(n, f) -> (n,) <$> getAllFilterByHelper showE tableRecordInfo f) filterTypeInfo
  let allFilters = map (\(n, f) -> UInfixE (LitE (StringL $ "" <> nameBase n)) (VarE $ mkName "++") (showFilters f)) allFilters'
  return allFilters
  where
    -- getAllFilterByHelper :: Name ->  [(Name, [(Name, Type)])]  ->  [(Name, Type)] -> Q [Maybe Exp]
    -- getAllFilterByHelper :: Name ->  [(Name, [(Name, Type)])]  ->  [(Name, Type)] -> Q [Maybe Exp]
    getAllFilterByHelper showE tableRecordInfo = mapM (\(f, _) -> getValueFromRecord showE f tableRecordInfo)
    showFilters =
      foldr
        (\curr prev -> UInfixE (UInfixE (LitE $ StringL "-") (VarE $ mkName "++") curr) (VarE $ mkName "++") prev)
            -- Nothing -> fail "showFilters")
        -- (\curr prev -> case curr of
        --     Just curr -> UInfixE (UInfixE (LitE $ StringL "-") (VarE $ mkName "++") curr) (VarE $ mkName "++") prev
        --     Nothing -> UInfixE (LitE $ StringL "-prev-") (VarE $ mkName "++") prev)
        --     -- Nothing -> fail "showFilters")
        (LitE (StringL ""))

-- FilterById {id :: Int} | FilterByName {name :: String} | FilterByIdAndName {id :: Int, name :: String}
--  [ (name , [ (name, type)], (name ,  [(name,type)],       (name,            [(name, type), (name, type)]   ]

-- Artist { id :: Int, name :: String}
-- [(name, [(name,type), (name, type)])]

-- create filterby from the records
-- find for each datacons

deriveFilterByClass :: Name -> Name -> Q [Dec]
deriveFilterByClass tableRecordName filterByName = do
  [d|
    instance FilterByClass $(conT tableRecordName) $(conT filterByName) where
      getAllKeys tableRecord Proxy = $(handleGetAllKeys' tableRecordName filterByName)
      getKey filterBy = $(handleGetKey filterByName)
    |]

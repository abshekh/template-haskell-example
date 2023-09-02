{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Types where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Data (Proxy (Proxy))

-- Define a custom data type
data MyEnum = A | B | C deriving (Show)

class Countable a where
  count :: Proxy a -> Integer

-- instance (Enum a, Bounded a) => Countable a where
--   count Proxy = fromIntegral $
--     1 + fromEnum (maxBound :: a) - fromEnum (minBound :: a)

class ToString a where
  toString :: Proxy a -> String
  showValue :: a -> String

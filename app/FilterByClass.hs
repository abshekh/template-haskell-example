{-# LANGUAGE AllowAmbiguousTypes #-}

module FilterByClass where
import Data.Data (Proxy)

class FilterByClass tableRecord filterBy | filterBy -> tableRecord where
  getAllKeys :: tableRecord -> Proxy filterBy -> [String]
  getKey :: filterBy -> String

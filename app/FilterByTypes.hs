module FilterByTypes where

-- import Language.Haskell.TH

import Data.Data (Proxy (Proxy))
import FilterByClass
import FilterByTH

data FilterBy
  = FilterById {artistId :: Int}
  | FilterByName {artistName :: String}
  | FilterByIdAndName {artistId :: Int, artistName :: String}
  -- | FilterByIds {artistIds :: [Int]}
  -- | FilterByIds' {artistIds' :: (Int, Int)}

data Artist = Artist
  { artistId :: Int,
    artistName :: String
  }

$(deriveFilterByClass ''Artist ''FilterBy)

artist = Artist 1 "Hello"

-- >>> getKey $ FilterById 1
-- "FilterById-1"

-- >>> getKey $ FilterByIdAndName 1 "Hello"
-- "FilterByIdAndName-1-\"Hello\""

-- >>> getKey $ FilterByName "Hello"
-- "FilterByName-\"Hello\""

-- >>> getKey $ FilterByIds [1, 2, 3]
-- "FilterByIds-[1,2,3]"

-- >>> getKey $ FilterByIds' (1,2)
-- "FilterByIds'-(1,2)"

-- >>> getAllKeys artist (Proxy :: Proxy FilterBy)
-- ["FilterById-1","FilterByName-\"Hello\"","FilterByIdAndName-1-\"Hello\""]


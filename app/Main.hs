module Main where

import Data.Data (Proxy (Proxy))
import Language.Haskell.TH
import THExample
import Types

two :: Int
two = $(add1 1)

main :: IO ()
main = putStrLn ($(pr "Hello"))

data X = Y | Z | A | B

data AA = AA
  { a :: Int,
    b :: Int
  } deriving (Show)

data T
  = V {c :: Int}
  | AB {d :: Int} deriving (Show)

deriveCountableSimple ''Bool
deriveToString ''Bool
deriveToString ''X
deriveToString'' ''T
-- deriveToString'' ''AA

-- >>> toString (Proxy :: Proxy T)
-- "[[\"V\",\"c\"],[\"AB\",\"d\"]]"


-- >>> showValue $ V 2
-- "2"

-- >>> showValue $ AB 1
-- "1"


-- >>> count (Proxy :: Proxy Bool)
-- 2

-- >>> toString (Proxy :: Proxy Bool)
-- "False + True + "

-- >>> toString (Proxy :: Proxy X)
-- "Y + Z + A + B + "

-- >>> toString (Proxy :: Proxy AA)
-- "[\"[\\\"AA\\\",\\\"a\\\",\\\"b\\\"]\"] + "

-- >>> $(deriveToString ''X)

module BPredictor.Utils (
  zipInits
, next
) where

import qualified Data.List as L

zipInits :: String -> [(String, String)]
zipInits xs = let xss = L.inits xs in L.zip xss (L.tail xss)

next :: String -> Char -> String
next xs x = L.tail (xs ++ [x])

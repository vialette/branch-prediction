module Main (main) where

import qualified Data.Foldable         as F

import qualified BPredictor.FST.BP2FST as BP2FST
import qualified BPredictor.FST.PFST   as PFST
import qualified BPredictor.FST.WFST   as WFST

doJob :: String -> String -> IO ()
doJob alph xs  = do
  putStr "\n"
  putStrLn $ "pattern=\""  ++ xs   ++ "\""
  putStrLn $ "alphabet=\"" ++ alph ++ "\""
  putStr "\n"
  putStrLn "2-bit predictor transducer"
  putStrLn "---------------"
  print $ BP2FST.mk
  putStr "\n"
  putStrLn "word transducer"
  putStrLn "---------------"
  print $ WFST.mk alph xs
  putStr "\n"
  putStrLn "product transducer"
  putStrLn "------------------"
  print $  PFST.mk alph xs
  putStr "\n"

main :: IO ()
main = do
  let alph = "abcd"
  F.sequence_ $ fmap (doJob alph) ["a","aa","ab","aaa","aab","aba","abb","abc","abab","abaa","ababa"]

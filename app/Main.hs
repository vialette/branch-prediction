module Main (main) where

import qualified Data.Foldable as F

import qualified BPredictor.FST.WFST as WFST
import qualified BPredictor.FST.PFST as PFST

doJob :: String -> String -> IO ()
doJob alph xs  = do
  putStr "\n"
  putStrLn $ "pattern=\""  ++ xs   ++ "\""
  putStrLn $ "alphabet=\"" ++ alph ++ "\""
  putStr "\n"
  putStrLn "word transducer"
  putStrLn "---------------"
  print $ WFST.mkFST alph xs
  putStr "\n"
  putStrLn "product transducer"
  putStrLn "------------------"
  print $  PFST.mkFST alph xs
  putStr "\n"

main :: IO ()
main = do
  let alph = "ab"
  F.sequence_ $ fmap (doJob alph) ["a", "aa", "ab", "aaa", "aab", "aba", "abb"]

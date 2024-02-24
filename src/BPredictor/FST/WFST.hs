module BPredictor.FST.WFST (
  -- * Types
  S
, T
, FST

  -- * Making
, mkFST

  --
, emptyString
) where

import           Control.Monad
import qualified Data.Foldable      as F
import qualified Data.List          as L
import           Data.Map.Strict    as M
import           Data.Maybe


import qualified BPredictor.FST.GFST   as GFST
import qualified BPredictor.FST.BP2FST as BP2FST
import           BPredictor.Nat
import qualified BPredictor.Utils      as Utils

type S = GFST.S String

type T = GFST.T String String

type FST = GFST.FST String Char String

emptyString :: String
emptyString = ""

initS :: S
initS = GFST.mkS emptyString

readFromInitA :: String -> FST -> Maybe ([T], S)
readFromInitA = GFST.readFST initS

mkBackboneA :: String -> FST
mkBackboneA = F.foldr step GFST.emptyFST . Utils.zipInits
  where
    step (xs, ys) = GFST.insertFST q x t
      where
        x  = L.last ys
        q  = GFST.mkS xs
        q' = GFST.mkS ys
        t  = GFST.mkT "1" q'

mkFST :: GFST.Alph -> String -> FST
mkFST alph xs = F.foldl step (mkBackboneA xs) . fmap GFST.mkS $ L.inits xs
  where
    step a q = F.foldl (goMkFST xs q) a alph

goMkFST :: String -> S -> FST -> Char -> FST
goMkFST xs q fST x = case GFST.transFST q x fST of
  Just _  -> fST
  Nothing -> GFST.insertFST q x t fST
    where
      t  = GFST.mkT l q'
        where
          l = (if GFST.getS q == xs && x == L.head xs then "10" else "0") ++
              F.concat (fmap GFST.getLabelT ts)
      Just (ts, q') = readFromInitA ys fST
        where
          ys = Utils.next (GFST.getS q) x

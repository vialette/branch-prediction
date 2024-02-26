module BPredictor.FST.WFST (
  -- * Types
  Q
, T
, FST

  -- * Making
, mk

  --
, emptyString
) where

import qualified Data.Foldable          as F
import qualified Data.List              as L
import           Data.Maybe

import qualified BPredictor.FST.GFST    as GFST
import qualified BPredictor.FST.Inner.Q as FST.Q
import qualified BPredictor.FST.Inner.T as FST.T
import qualified BPredictor.Utils       as Utils

-- Specialization
type A = String
type R = Char
type W = String

-- |Type of state
type Q = FST.Q.Q A

-- |Type of transition
type T = FST.T.T A R W

-- |Type of finite state transducer
type FST = GFST.FST A R W

emptyString :: String
emptyString = ""

-- Finite state transducer with one state corresponding to the empty string
-- and no transition.
initS :: Q
initS = FST.Q.mk emptyString

-- Specialize the function GFST.readFST for reading from the initial state
-- (i.e. the state corresponding to the empty string).
readFromInitA :: [R] -> FST -> Maybe ([T], Q)
readFromInitA = GFST.readFST initS

mkBackboneA :: [R] -> FST
mkBackboneA = F.foldr step GFST.emptyFST . Utils.zipInits
  where
    step (xs, ys) = GFST.insertFST (FST.T.mk qFrom r w qTo)
      where
        qFrom = FST.Q.mk xs
        r     = L.last ys
        w     = "1"
        qTo   = FST.Q.mk ys

-- |The 'mkFst' function returns the word transducer that correspond to a given pattern.
mk :: [R] -> [R] -> FST
mk alph xs = F.foldl step (mkBackboneA xs) . fmap FST.Q.mk $ L.inits xs
  where
    step wFST q = F.foldl (goMk xs q) wFST alph

goMk :: [R] -> Q -> FST -> Char -> FST
goMk xs qFrom wFST r = case GFST.tFST qFrom r wFST of
  Just _  -> wFST
  Nothing -> GFST.insertFST t wFST
    where
      t  = FST.T.mk qFrom r w qTo
        where
          w = (if FST.Q.getQ qFrom == xs && r == L.head xs then "10" else "0") ++ F.concat (fmap FST.T.getW ts)
      -- readFromInitA cannot fail for complete transducers
      (ts, qTo) = fromJust $ readFromInitA ys wFST
        where
          ys = Utils.next (FST.Q.getQ qFrom) r

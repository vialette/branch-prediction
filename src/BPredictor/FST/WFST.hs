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

mkPath :: [R] -> FST
mkPath = F.foldr step GFST.emptyFST . L.init . Utils.zipInits
  where
    step (xs, ys) = GFST.insertFST t
      where
        t = FST.T.mk qFrom r w qTo
          where
            qFrom = FST.Q.mk xs
            r     = L.last ys
            w     = "1"
            qTo   = FST.Q.mk ys

-- |The 'mkFst' function returns the word transducer that correspond to a given pattern.
mk :: [R] -> [R] -> FST
mk alph xs = F.foldl step (mkPath xs) . fmap FST.Q.mk . L.init $ L.inits xs
  where
    step wFST qFrom = F.foldl (stepMk xs qFrom) wFST alph

stepMk :: [R] -> Q -> FST -> Char -> FST
stepMk xs qFrom wFST r = case GFST.tFST qFrom r wFST of
  Just _  -> wFST
  Nothing -> GFST.insertFST t wFST
    where
      t  = FST.T.mk qFrom r w qTo
        where
          w = (if FST.Q.getQ qFrom == L.init xs && r == L.head xs then "10" else "0") ++ F.concat (fmap FST.T.getW ts)
      -- readFromInitA cannot fail for complete transducers
      (ts, qTo) = fromJust $ GFST.readFST initS ys wFST
        where
          ys = Utils.next (FST.Q.getQ qFrom) r

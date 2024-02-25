module BPredictor.FST.WFST (
  -- * Types
  Q
, T
, FST

  -- * Making
, mkFST

  --
, emptyString
) where

import qualified Data.Foldable         as F
import qualified Data.List             as L
import           Data.Maybe

import qualified BPredictor.FST.GFST   as GFST
import qualified BPredictor.Utils      as Utils

-- |Type of state
type Q = GFST.Q String

-- |Type of transition
type T = GFST.T String String

-- |Type of finite state transducer
type FST = GFST.FST String Char String

emptyString :: String
emptyString = ""

-- Finite state transducer with one state corresponding to the empty string
-- and no transition.
initS :: Q
initS = GFST.mkQ emptyString

-- Specialize the function GFST.readFST for reading from the initial state
-- (i.e. the state corresponding to the empty string).
readFromInitA :: String -> FST -> Maybe ([T], Q)
readFromInitA = GFST.readFST initS

mkBackboneA :: String -> FST
mkBackboneA = F.foldr step GFST.emptyFST . Utils.zipInits
  where
    step (xs, ys) = GFST.insertFST q x t
      where
        x  = L.last ys
        q  = GFST.mkQ xs
        q' = GFST.mkQ ys
        t  = GFST.mkT "1" q'

-- |The 'mkFst' function returns the word transducer that correspond to a given pattern.
mkFST :: GFST.Alph -> String -> FST
mkFST alph xs = F.foldl step (mkBackboneA xs) . fmap GFST.mkQ $ L.inits xs
  where
    step wFST q = F.foldl (goMkFST xs q) wFST alph

goMkFST :: String -> Q -> FST -> Char -> FST
goMkFST xs q wFST x = case GFST.transFST q x wFST of
  Just _  -> wFST
  Nothing -> GFST.insertFST q x t wFST
    where
      t  = GFST.mkT l q'
        where
          l = (if GFST.getQ q == xs && x == L.head xs then "10" else "0") ++
              F.concat (fmap GFST.getWriteT ts)
      -- readFromInitA cannot fail for complete transducers
      (ts, q') = fromJust $ readFromInitA ys wFST
        where
          ys = Utils.next (GFST.getQ q) x

module BPredictor.FST.BP2FST (
  -- * Types
  BP(..)
, Q
, T
, FST

  -- * Making
, mkFST
) where

import qualified Data.Foldable      as F
import           Data.Tuple.Extra

import qualified BPredictor.FST.GFST as GFST
import           BPredictor.Nat

data BP = Nu2 | Nu1 | Tau1 | Tau2 deriving (Show, Eq, Ord)

-- |Type of state
type Q = GFST.Q BP

-- |Type of transition
type T = GFST.T BP Nat

-- |Type of finite state transducer
type FST = GFST.FST BP Char Nat

match :: Char
match = '1'

mismatch :: Char
mismatch = '0'

mkFST :: FST
mkFST = F.foldr (uncurry3 GFST.insertFST) GFST.emptyFST ts
  where
    qNu2  = GFST.mkQ  Nu2
    qNu1  = GFST.mkQ  Nu1
    qTau1 = GFST.mkQ Tau1
    qTau2 = GFST.mkQ Tau2

    ts  = [ ( qNu2, mismatch, GFST.mkT 0  qNu2)
          , ( qNu2,    match, GFST.mkT 1  qNu1)
          , ( qNu1, mismatch, GFST.mkT 0  qNu2)
          , ( qNu1,    match, GFST.mkT 1 qTau1)
          , (qTau1, mismatch, GFST.mkT 1  qNu1)
          , (qTau1,    match, GFST.mkT 0 qTau2)
          , (qTau2, mismatch, GFST.mkT 1 qTau1)
          , (qTau2,    match, GFST.mkT 0 qTau2)]

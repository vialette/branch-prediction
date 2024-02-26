module BPredictor.FST.BP2FST (
  -- * Types
  BP(..)
, Q
, T
, FST

  -- * Making
, mk
) where

import qualified Data.Foldable          as F

import qualified BPredictor.FST.GFST    as GFST
import qualified BPredictor.FST.Inner.Q as FST.Q
import qualified BPredictor.FST.Inner.T as FST.T
import           BPredictor.Nat

data BP = Nu2 | Nu1 | Tau1 | Tau2 deriving (Show, Eq, Ord)

-- |Type of state
type Q = FST.Q.Q BP

-- |Type of transition
type T = FST.T.T BP Nat

-- |Type of finite state transducer
type FST = GFST.FST BP Char Nat

match :: Char
match = '1'

mismatch :: Char
mismatch = '0'

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

mk :: FST
mk = F.foldr GFST.insertFST GFST.emptyFST ts
  where
    qNu2  = FST.Q.mk  Nu2
    qNu1  = FST.Q.mk  Nu1
    qTau1 = FST.Q.mk Tau1
    qTau2 = FST.Q.mk Tau2

    ts  = fmap (uncurry4 FST.T.mk)[ ( qNu2, mismatch, 0,  qNu2)
                                  , ( qNu2,    match, 1,  qNu1)
                                  , ( qNu1, mismatch, 0,  qNu2)
                                  , ( qNu1,    match, 1, qTau1)
                                  , (qTau1, mismatch, 1,  qNu1)
                                  , (qTau1,    match, 0, qTau2)
                                  , (qTau2, mismatch, 1, qTau1)
                                  , (qTau2,    match, 0, qTau2)]

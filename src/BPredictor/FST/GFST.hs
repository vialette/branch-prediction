module BPredictor.FST.GFST (
  -- * Types
  Q(..)
, T(..)
, FST(..)
, Alph

  -- * Making
, emptyFST
, mkQ
, mkT

  -- automata
, transFST
, readFST
, readFST'
, insertFST
, qsFST

  -- * Transitions
, getOutputT
, getQT
) where

import           Control.Arrow
import qualified Data.Foldable      as F
import qualified Data.List          as L
import           Data.Map.Strict    as M
import           Data.Maybe

type Alph = String

-- State of finite state transducers
newtype Q a = Q { getQ :: a } deriving (Eq, Ord)

-- Transition of finite state transducers
newtype T a o = T { getT :: (o, Q a) } deriving (Show, Eq, Ord)

type KFST a     = (Q a)
type VFST a i o = M.Map i (T a o)

-- Finit state transducer
newtype FST a i o = FST { getM :: M.Map (KFST a) (VFST a i o) }

instance Show a => Show (Q a) where
  show Q { getQ = q } = show q

instance (Show a, Show i, Show o, Ord a) => Show (FST a i o) where
  show fst@FST { getM = m } =
    L.intercalate "\n" [showQ q | q <- qsFST fst]
      where
        showQ q = "state=" ++ show q ++ " transitions=[" ++ showT q ++ "]"
        showT q = L.intercalate "," [goShowT t | let Just m' = M.lookup q m
                                               , t <- M.assocs m']
          where
            goShowT (x, t) = "(char="  ++ show x              ++
                             ",label=" ++ show (getOutputT t) ++
                             ",state=" ++ show (getQT t)  ++ ")"

getOutputT :: T a o -> o
getOutputT (T t) = fst t

getQT :: T a o -> Q a
getQT (T t) = snd t

emptyFST :: FST a i o
emptyFST = FST { getM = M.empty }

mkQ :: a -> Q a
mkQ q = Q { getQ = q }

mkT :: o -> Q a -> T a o
mkT o q =  T { getT = (o, q) }

qsFST :: (Ord a) => FST a i o -> [Q a]
qsFST = L.sort . M.keys . getM

transFST :: (Ord a, Ord i) => Q a -> i -> FST a i o -> Maybe (T a o)
transFST q x fST = M.lookup q (getM fST) >>= M.lookup x

readFST :: (Ord a, Ord i) => Q a -> [i] -> FST a i o -> Maybe ([T a o], Q a)
readFST q xs fST = F.foldl step acc0 xs >>= (Just . first L.reverse)
  where
    acc0 = Just ([], q)

    step Nothing    _ = Nothing
    step (Just acc) x = transFST (snd acc) x fST >>= updateAcc
      where
        updateAcc t = Just (t : fst acc, getQT t)

readFST' :: (Ord a, Ord i) => Q a -> [i] -> FST a i o -> Maybe ([o], Q a)
readFST' q xs fST = readFST q xs fST >>= (Just . first (fmap getOutputT))

insertFST :: (Ord a, Ord i) => Q a -> i -> T a o -> FST a i o -> FST a i o
insertFST q x t FST { getM = m } = FST { getM = m' }
  where
    m' = case M.lookup q m of
      Nothing -> M.update f q (M.insert q M.empty m)
      Just _  -> M.update f q m
      where
        f = Just . M.insert x t

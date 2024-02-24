module BPredictor.FST.GFST (
  -- * Types
  S(..)
, T(..)
, FST(..)
, Alph

  -- * Making
, emptyFST
, mkS
, mkT

  -- automata
, transFST
, availabletransFST
, readFST
, readFST'
, insertFST
, statesFST

  -- * Transitions
, getLabelT
, getStateT
) where

import           Control.Arrow
import qualified Data.Foldable      as F
import qualified Data.List          as L
import           Data.Map.Strict    as M
import           Data.Maybe

type Alph = String

newtype S a = S { getS :: a } deriving (Eq, Ord)

newtype T a output = T { getT :: (output, S a) } deriving (Show, Eq, Ord)

type TS a input output = M.Map input (T a output)

newtype FST a input output = FST { getM :: M.Map (S a) (TS a input output) }

instance Show a => Show (S a) where
  show S { getS = a } = show a

instance (Show a, Show input, Show output, Ord a) => Show (FST a input output) where
  show fst@FST { getM = m } =
    L.intercalate "\n" [showS q | q <- statesFST fst]
      where
        showS q = "state=" ++ show q ++ " transitions=[" ++ showT q ++ "]"
        showT q = L.intercalate "," [goShowT t | let Just m' = M.lookup q m
                                               , t <- M.assocs m']
          where
            goShowT (x, t) = "(char="  ++ show x            ++
                             ",label=" ++ show (getLabelT t) ++
                             ",state=" ++ show (getStateT t) ++ ")"

getLabelT :: T a output -> output
getLabelT (T t) = fst t

getStateT :: T a output -> S a
getStateT (T t) = snd t

emptyFST :: FST a input output
emptyFST = FST { getM = M.empty }

mkS :: a -> S a
mkS a = S { getS = a }

mkT :: output -> S a -> T a output
mkT o q =  T { getT = (o, q) }

statesFST :: (Ord a) => FST a input output -> [S a]
statesFST = L.sort . M.keys . getM

transFST :: (Ord a, Ord input) => S a -> input -> FST a input output -> Maybe (T a output)
transFST q x fST = M.lookup q (getM fST) >>= M.lookup x

availabletransFST :: (Ord a) => S a -> FST a input output -> Maybe [(input, T a output)]
availabletransFST q fST = M.lookup q (getM fST) >>= (Just . M.assocs)

readFST :: (Ord a, Ord input) => S a -> [input] -> FST a input output -> Maybe ([T a output], S a)
readFST q xs fST = F.foldl step acc0 xs >>= (Just . first L.reverse)
  where
    acc0 = Just ([], q)

    step Nothing    _ = Nothing
    step (Just acc) x = transFST (snd acc) x fST >>= updateAcc
      where
        updateAcc t = Just (t : fst acc, getStateT t)

readFST' :: (Ord a, Ord input) => S a -> [input] -> FST a input output -> Maybe ([output], S a)
readFST' q xs fst = readFST q xs fst >>= (Just . first (fmap getLabelT))

insertFST :: (Ord a, Ord input) => S a -> input -> T a output -> FST a input output -> FST a input output
insertFST q x t FST { getM = m } = FST { getM = m' }
  where
    m' = case M.lookup q m of
      Nothing -> M.update f q (M.insert q M.empty m)
      Just _  -> M.update f q m
      where
        f = Just . M.insert x t

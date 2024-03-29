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

  -- £ Querying
, transFST
, readFST
, readFST'
, insertFST
, qsFST

  -- * Transitions
, getWriteT
, getTargetStateT
) where

import           Control.Arrow
import qualified Data.Foldable      as F
import qualified Data.List          as L
import           Data.Map.Strict    as M

-- Alphabet
type Alph = String

-- State of finite state transducers
newtype Q a = Q { getQ :: a } deriving (Eq, Ord)

-- Transition of finite state transducers
newtype T a w = T { getT :: (w, Q a) } deriving (Show, Eq, Ord)

type KFST a     = (Q a)
type VFST a r w = M.Map r (T a w)

-- Finit state transducer
newtype FST a r w = FST { getM :: M.Map (KFST a) (VFST a r w) }

-- type KFST a r   = (Q a,   r)
-- type VFST a r w = (  w, Q a)
-- type FST  a r w = M.Map (KFST a r) (VFST a r w) 

instance Show a => Show (Q a) where
  show Q { getQ = q } = show q

instance (Show a, Show r, Show w, Ord a) => Show (FST a r w) where
  show fST@FST { getM = m } =
    L.intercalate "\n" [showQ q | q <- qsFST fST]
      where
        showQ q = L.intercalate "\n" [showT t | let Just m' = M.lookup q m, t <- M.assocs m']
          where
            showT (x, t) = "q:" ++ show q ++
                           " --- " ++ show x ++ "/" ++ show (getWriteT t) ++ " --> " ++
                           "q:" ++ show (getTargetStateT t)

-- |The 'getWriteT' function returns the write part of a transition.
getWriteT :: T a w -> w
getWriteT (T t) = fst t

-- |The 'getTargetStateT' function returns the target state of a transition.
getTargetStateT :: T a w -> Q a
getTargetStateT (T t) = snd t

emptyFST :: FST a r w
emptyFST = FST { getM = M.empty }

-- |The 'mkQ' functions returns a new state.
mkQ :: a -> Q a
mkQ q = Q { getQ = q }

-- |The 'mkT' functions returns a new transition.
mkT :: w -> Q a -> T a w
mkT w q =  T { getT = (w, q) }

-- |The 'qsFST' functions returns all states of a finite state transducer.
qsFST :: (Ord a) => FST a r w -> [Q a]
qsFST = L.sort . M.keys . getM

-- |The 'transFST' function returns a transition from a source state, a read, a write and
-- a target state. 
transFST :: (Ord a, Ord r) => Q a -> r -> FST a r w -> Maybe (T a w)
transFST q x fST = M.lookup q (getM fST) >>= M.lookup x

readFST :: (Ord a, Ord r) => Q a -> [r] -> FST a r w -> Maybe ([T a w], Q a)
readFST q xs fST = F.foldl step acc0 xs >>= (Just . first L.reverse)
  where
    acc0 = Just ([], q)

    step Nothing    _ = Nothing
    step (Just acc) x = transFST (snd acc) x fST >>= updateAcc
      where
        updateAcc t = Just (t : fst acc, getTargetStateT t)

readFST' :: (Ord a, Ord r) => Q a -> [r] -> FST a r w -> Maybe ([w], Q a)
readFST' q xs fST = readFST q xs fST >>= (Just . first (fmap getWriteT))

-- |The 'insertFST' fucntion inserts a transition in a finite state transducer.
-- If a transition with the same source state and the same read is already in
-- the finite state transducer, it is replaced.
insertFST :: (Ord a, Ord r) => Q a -> r -> T a w -> FST a r w -> FST a r w
insertFST q x t FST { getM = m } = FST { getM = m' }
  where
    m' = case M.lookup q m of
      Nothing -> M.update f q (M.insert q M.empty m)
      Just _  -> M.update f q m
      where
        f = Just . M.insert x t

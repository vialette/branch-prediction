module BPredictor.FST.GFST (
  -- * Types
  FST(..)

  -- * Making
, emptyFST

  -- * Querying
, tFST
, tFST'
, readFST
, readFST'
, insertFST
, qsFST
) where

import           Control.Arrow
import qualified Data.Foldable          as F
import qualified Data.List              as L
import qualified Data.List.Extra        as L.X

import qualified BPredictor.FST.Inner.Q as FST.Q
import qualified BPredictor.FST.Inner.T as FST.T

-- Finit state transducer
newtype FST a r w = FST { getTs :: [FST.T.T a r w] }

instance (Show a, Show r, Show w, Ord a, Ord r) => Show (FST a r w) where
  show = F.foldl step [] . F.concat . tsByQFST
    where
      step acc = (++) acc . showT
        where
          showT t = "q:"    ++ show (FST.T.getQFrom t) ++
                    "\t--- " ++ show (FST.T.getR t)    ++
                               "/"                     ++
                               show (FST.T.getW t)     ++
                    " --> \t"                          ++
                    "q:"    ++ show (FST.T.getQTo t)   ++
                    "\n"

-- Insert a transition in a finite state transducers.
-- Transition are sorted increasingly.
insertT :: (Ord a, Ord r, Ord w) => FST.T.T a r w -> [FST.T.T a r w] -> [FST.T.T a r w]
insertT t = go
  where
    qFrom = FST.T.getQFrom t
    r     = FST.T.getR     t
    
    go []                          = [t]
    go (t' : ts)
      | qFrom  < qFrom'            = t : t' : ts
      | qFrom == qFrom' && r  < r' = t : t' : ts
      | qFrom == qFrom' && r == r' = t : ts
      | otherwise                  = t' : go ts
      where
        qFrom' = FST.T.getQFrom t'
        r'     = FST.T.getR     t'

-- Search for transition in a finite state transducer.
-- Transition are sorted increasingly.
lookupT :: (Ord a, Ord r) => FST.Q.Q a -> r -> [FST.T.T a r w] -> Maybe (FST.T.T a r w)
lookupT _     _ []             = Nothing
lookupT qFrom r (t : ts)
  | qFrom  < qFrom'            = Nothing
  | qFrom == qFrom' && r == r' = Just t
  | otherwise                  = lookupT qFrom r ts
  where
    qFrom' = FST.T.getQFrom t
    r'     = FST.T.getR     t

-- |The 'qsFST' functions returns all states of a finite state transducer.
qsFST :: (Ord a) => FST a r w -> [FST.Q.Q a]
qsFST = fmap L.head . L.group . L.sort . fmap FST.T.getQFrom . getTs

-- |The 'tsByQFST' resturns all transtitions sorted by source state.
tsByQFST :: (Ord a, Ord r) => FST a r w -> [[FST.T.T a r w]]
tsByQFST = fmap (L.sortOn FST.T.getR) . L.X.groupOn FST.T.getQFrom . L.sortOn FST.T.getQFrom . getTs

-- |The 'tFST' function operates a transition from a state in a finite state transducer.
tFST :: (Ord a, Ord r) => FST.Q.Q a -> r -> FST a r w -> Maybe (FST.T.T a r w)
tFST qFrom r = lookupT qFrom r . getTs

-- |The 'tFST'' function operates a transition from a state in a finite state transducer.
tFST' :: (Ord a, Ord r) => FST.Q.Q a -> r -> FST a r w -> Maybe (w, FST.Q.Q a)
tFST' qFrom r fST = tFST qFrom r fST >>= (Just . (FST.T.getW &&& FST.T.getQTo))

readFST :: (Ord a, Ord r) => FST.Q.Q a -> [r] -> FST a r w -> Maybe ([FST.T.T a r w], FST.Q.Q a)
readFST qFrom rs fST = F.foldl step acc0 rs >>= (Just . first L.reverse)
  where
    acc0 = Just ([], qFrom)
    step Nothing    _ = Nothing
    step (Just acc) r = tFST (snd acc) r fST >>= (\t -> Just (t : fst acc, FST.T.getQTo t))

readFST' :: (Ord a, Ord r) => FST.Q.Q a -> [r] -> FST a r w -> Maybe ([w], FST.Q.Q a)
readFST' qFrom r fST = readFST qFrom r fST >>= (Just . first (fmap FST.T.getW))

-- |The 'insertFST' function insert a
insertFST :: (Ord a, Ord r, Ord w) => FST.T.T a r w -> FST a r w -> FST a r w
insertFST t FST { getTs = ts } = FST { getTs = insertT t ts }

emptyFST :: FST a r w
emptyFST = FST { getTs = [] }

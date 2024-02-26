module BPredictor.FST.Inner.T (
  -- * Type
  T(..)

  -- * Making
, mk
) where

import qualified BPredictor.FST.Inner.Q as FST.Q

data T a r w = T { getQFrom :: FST.Q.Q a
                 , getR     :: r
                 , getW     :: w
                 , getQTo   :: FST.Q.Q a
                 } deriving (Show, Eq, Ord)

-- |The 'mkT' functions returns a new transition.
mk :: FST.Q.Q a -> r -> w -> FST.Q.Q a -> T a r w
mk qFrom r w qTo = T { getQFrom = qFrom, getR = r, getW = w, getQTo = qTo }

module BPredictor.FST.Inner.Q (
  -- * Type
  Q(..)

  -- * Making
, mk
) where

-- State of finite state transducers
newtype Q a = Q { getQ :: a } deriving (Eq, Ord)

instance Show a => Show (Q a) where
  show Q { getQ = q } = show q

mk :: a -> Q a
mk a = Q { getQ = a }

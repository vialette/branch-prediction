module BPredictor where

import           Control.Monad
import qualified Data.Foldable      as F
import qualified Data.Function.Slip as F.Slip
import qualified Data.List          as L
import           Data.Map.Strict    as M
import           Data.Maybe
import qualified Data.Set           as S

type Alphabet = String

type State = String

type K = (State, Char)

type T a = (a, State)

data A a = A { q0 :: State, trans :: M.Map K (T a) } deriving Show

nubStrings :: [String] -> [String]
nubStrings = L.sort . S.toList . S.fromList

zipInits :: String -> [(String, String)]
zipInits xs = let xss = L.inits xs in L.zip xss (L.tail xss)

initS :: State
initS = ""

statesA :: A a -> [String]
statesA = nubStrings . fmap fst . M.keys . trans

transA :: K -> A a -> Maybe State
transA k a = M.lookup k (trans a) >>= (Just . snd)

readA ::  State -> String -> A a -> Maybe State
readA xs ys a = F.foldl step (Just xs) ys
  where
    step Nothing    _ = Nothing
    step (Just xs') y = transA (xs', y) a

q0ReadA :: String -> A a -> Maybe State
q0ReadA = readA initS

insertA :: K -> T a -> A a -> A a
insertA k v a = a { trans = M.insert k v (trans a) }

emptyA :: A a
emptyA = A { q0 = initS, trans = M.empty }

mkLineA :: String -> A ()
mkLineA = F.foldr step emptyA . zipInits
  where
    step (xs, ys) = insertA (xs, L.last ys) ((), ys)

mkA :: Alphabet -> String -> A ()
mkA alph xs = F.foldr step (mkLineA xs) $ L.inits xs
  where
    step xs' a = F.foldr step' a alph
      where
        step' x a' = case transA (xs', x) a' of
            Nothing -> insertA (xs', x) ((), xs'') a'
            Just _  -> a'
            where
              Just xs'' = q0ReadA (L.tail (xs' ++ [x])) a'

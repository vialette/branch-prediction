module Auto where

import           Control.Monad
import qualified Data.Foldable      as F
import qualified Data.List          as L
import           Data.Map.Strict    as M
import           Data.Maybe
import qualified Data.Set           as S

type Alphabet = String

-- state
type S = String

-- transition
type T = String

-- map key
type K = (S, Char)

-- value key
type V = (T, S)

-- automata
data A = A { q0 :: S, trans :: M.Map K V }

instance Show A where
  show a@A { q0 = q, trans = m } =
    "initial state=" ++ show q ++ "\n" ++
    L.intercalate "\n" [showState q ++ " = [" ++ showEdges q vs ++ "]" | q <- statesA a]
      where
        vs = M.assocs m
        showState q = "state=" ++ show q
        showEdges q = L.intercalate "," . fmap f . L.filter ((== q) . fst . fst)
          where
            f ((_, x), (t, q')) = "(char=" ++ show x ++ ",label=" ++ t ++ ",state=" ++ show q' ++ ")"

zipInits :: String -> [(String, String)]
zipInits xs = let xss = L.inits xs in L.zip xss (L.tail xss)

next :: String -> Char -> String
next xs x = L.tail (xs ++ [x])

initS :: S
initS = ""

statesA :: A -> [S]
statesA = L.sort . L.nub . fmap fst . M.keys . trans

transA :: K -> A -> Maybe (T, S)
transA k a = M.lookup k (trans a) >>= Just

readA :: S -> String -> A -> Maybe ([T], S)
readA q xs a = F.foldl step (Just ([], q)) xs >>= (\(ts, q') -> Just (L.reverse ts, q'))
  where
    step Nothing         _ = Nothing
    step (Just (ts, q')) x = case transA (q', x) a of
      Nothing       -> Nothing
      Just (t, q'') -> Just (t : ts, q'')

q0ReadA :: String -> A -> Maybe ([T], S)
q0ReadA = readA initS

insertA :: K -> V -> A -> A
insertA k v a = a { trans = M.insert k v (trans a) }

emptyA :: A
emptyA = A { q0 = initS, trans = M.empty }

mkLineA :: String -> A
mkLineA = F.foldr step emptyA . zipInits
  where
    step (xs, ys) = insertA (xs, L.last ys) ("1", ys)

mkA :: Alphabet -> String -> A
mkA alph xs = F.foldl step (mkLineA xs) $ L.inits xs
  where
    step a q = F.foldl step' a alph
      where
        step' a' x = case transA (q, x) a' of
          Nothing -> insertA (q, x) (ts', q') a'
          Just _  -> a'
          where
            Just (ts, q') = q0ReadA (next q x) a'
            ts' = (if q == xs && x == L.head xs then "10" else "0") ++ F.concat ts

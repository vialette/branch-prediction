
import qualified Data.Foldable      as F
import qualified Data.Function.Slip as F.Slip
import qualified Data.List          as L
import           Data.Map.Strict    as M
import           Data.Maybe
import qualified Data.Set           as S

type State = String

type K = (State, Char)

type T a = (a, State)

data A a = A { initS :: State, trans :: M.Map K (T a) } deriving Show

nubStrings :: [String] -> [String]
nubStrings = L.sort . S.toList . S.fromList

statesA :: A a -> [String]
statesA = nubStrings . fmap fst . M.keys . trans

transA :: State -> Char -> A a -> State
transA xs x = fromJust . M.lookup (xs, x) . trans

readA ::  State -> String -> A a -> State
readA xs ys a = F.foldl (F.Slip.slipl transA a) xs ys

-- mkA :: String -> String -> A ()
-- mkA alph xs = A . go $ fmap mkS L.inits xs
--   where
--     go []         = []
--     go [xs]       =
--     go (xs : xss) = [T (xs, a, xs ++ [a]) | a <- alph]

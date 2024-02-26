module BPredictor.FST.PFST (
  -- * Types
  Q
, T
, FST

  -- * Making
, mkFST
) where

import qualified Data.Foldable         as F
import qualified Data.List             as L

import qualified BPredictor.FST.GFST   as GFST
import qualified BPredictor.FST.BP2FST as BP2FST
import qualified BPredictor.FST.WFST   as WFST
import           BPredictor.Nat

-- |Type of state
type Q = GFST.Q (WFST.Q, BP2FST.Q)

-- |Type of transition
type T = GFST.T (WFST.Q, BP2FST.Q) Nat

-- |Type of finite state transducer
type FST = GFST.FST (WFST.Q, BP2FST.Q) Char Nat

mkFST :: GFST.Alph -> String -> FST
mkFST alph xs = goMkFST [initQ] GFST.emptyFST
  where
    -- the associated word finite state tranducer
    wFST   = WFST.mkFST   alph xs

    -- the associated 2-bit predictor finite state transducer
    bp2FST = BP2FST.mkFST

    -- the initial state of wFST
    initQWFST   = GFST.mkQ WFST.emptyString

    -- the initial state of bp2FST
    initQBP2FST = GFST.mkQ BP2FST.Nu2

    -- the initial state of the product finite state transducer
    initQ :: Q
    initQ = GFST.mkQ (initQWFST, initQBP2FST)

    -- develop states of the product finite state transducer
    goMkFST :: [Q] -> FST -> FST
    goMkFST []                    pFST = pFST
    goMkFST (q : qs)  pFST
      | q `L.elem` GFST.qsFST pFST     = goMkFST qs           pFST
      | otherwise                      = goMkFST (qs'' ++ qs) pFST'
      where
        qWFST   = fst $ GFST.getQ q
        qBP2FST = snd $ GFST.getQ q

        -- develop the current state of the product finite state transducer
        -- for every character of the alphabet
        (qs'', pFST') = F.foldr step ([], pFST) alph
          where
            step :: Char -> ([Q], FST) -> ([Q], FST)
            step x (qs', pFSTStep) = (q' : qs', pFSTStep')
              where
                pFSTStep' = GFST.insertFST q x t pFSTStep
                Just tWFST           = GFST.transFST qWFST   x  wFST
                Just (ws', qBP2FST') = GFST.readFST' qBP2FST ws bp2FST
                  where
                    ws = GFST.getWriteT tWFST
                q' = GFST.mkQ (GFST.getTargetStateT tWFST, qBP2FST')
                t  = GFST.mkT (F.sum ws') q'

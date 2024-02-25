module BPredictor.FST.PFST (
  -- * Types
  Q
, T
, FST

  -- * making
, mkFST
) where

import qualified BPredictor.FST.GFST   as GFST
import qualified BPredictor.FST.BP2FST as BP2FST
import qualified BPredictor.FST.WFST   as WFST
import           BPredictor.Nat

type Q = GFST.Q (WFST.Q, BP2FST.Q)

type T = GFST.T (WFST.Q, BP2FST.Q) Nat

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
      | q `L.elem` GFST.qsFST pFST     = goMkFST qs          pFST
      | otherwise                      = goMkFST (qs' ++ qs) pFST'
      where
        qWFST   = fst $ GFST.getQ q
        qBP2FST = snd $ GFST.getQ q

        -- develop the current state of the product finite state transducer
        -- for every character of the alphabet
        (qs', pFST') = F.foldr step ([], pFST) alph
          where
            step :: Char -> ([Q], FST) -> ([Q], FST)
            step x (qs, pFSTStep) = (q' : qs, pFSTStep')
              where
                pFSTStep' = GFST.insertFST q x t pFSTStep
                Just tWFST           = GFST.transFST qWFST   x  wFST
                Just (os', qBP2FST') = GFST.readFST' qBP2FST os bp2FST
                  where
                    os = GFST.getOutputT tWFST
                q' = GFST.mkQ (GFST.getQT tWFST, qBP2FST')
                t  = GFST.mkT (F.sum os') q'

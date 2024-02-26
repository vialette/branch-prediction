module BPredictor.FST.PFST (
  -- * Types
  Q
, T
, FST

  -- * Making
, mk
) where

import qualified Data.Foldable          as F
import qualified Data.List              as L

import qualified BPredictor.FST.BP2FST  as BP2FST
import qualified BPredictor.FST.GFST    as GFST
import qualified BPredictor.FST.Inner.Q as FST.Q
import qualified BPredictor.FST.Inner.T as FST.T
import qualified BPredictor.FST.WFST    as WFST
import           BPredictor.Nat

type A = (WFST.Q, BP2FST.Q)
type R = Char
type W = Nat

-- |Type of state
type Q = FST.Q.Q A

-- |Type of transition
type T = FST.T.T A R W

-- |Type of finite state transducer
type FST = GFST.FST A R W

mk :: [R] -> [R] -> FST
mk alph xs = goMk [initQ] GFST.emptyFST
  where
    -- the associated word finite state transducer
    wFST   = WFST.mk alph xs

    -- the associated 2-bit predictor finite state transducer
    bp2FST = BP2FST.mk

    -- the initial state of wFST
    initqFromWFST   = FST.Q.mk WFST.emptyString

    -- the initial state of bp2FST
    initqFromBP2FST = FST.Q.mk BP2FST.Nu2

    -- the initial state of the product finite state transducer
    initQ :: Q
    initQ = FST.Q.mk (initqFromWFST, initqFromBP2FST)

    -- develop states of the product finite state transducer
    goMk :: [Q] -> FST -> FST
    goMk []            pFST            = pFST
    goMk (qFrom : qs)  pFST
      | qFrom `L.elem` GFST.qsFST pFST = goMk qs           pFST
      | otherwise                      = goMk (qs'' ++ qs) pFST'
      where
        qFromWFST   = fst $ FST.Q.getQ qFrom
        qFromBP2FST = snd $ FST.Q.getQ qFrom

        -- develop the current state of the product finite state transducer
        -- for every character of the alphabet
        (qs'', pFST') = F.foldr step ([], pFST) alph
          where
            step :: R -> ([Q], FST) -> ([Q], FST)
            step r (qs', pFSTStep) = (qTo : qs', pFSTStep')
              where
                pFSTStep' = GFST.insertFST t pFSTStep
                Just tWFST               = GFST.tFST qFromWFST r wFST
                Just (ws', qFromBP2FST') = GFST.readFST' qFromBP2FST ws bp2FST
                  where
                    ws = FST.T.getW tWFST
                qTo = FST.Q.mk (FST.T.getQTo tWFST, qFromBP2FST')
                t   = FST.T.mk qFrom r (F.sum ws') qTo

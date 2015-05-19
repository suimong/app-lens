{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Example.Bench where

import Data.ApplicativeLens
import Examples.Evaluator 

import Control.DeepSeq

import Criterion.Main

instance NFData a => NFData (Env a) where
  rnf (Env xs) = rnf xs

instance NFData a => NFData (Val a) where
  rnf (VNum a)       = rnf a
  rnf (VFun s e env) = rnf s `seq` rnf e `seq` rnf env

instance NFData Exp where
  rnf (ENum i)     = rnf i
  rnf (EInc e)     = rnf e
  rnf (EFun s e)   = rnf s `seq` rnf e
  rnf (EApp e1 e2) = rnf e1 `seq` rnf e2
  rnf (EVar e)     = rnf e


main = defaultMain [
  bgroup "evalL" [ bench "E0"  $ nf (put (evalL expr) env0)      (VNum 65536)
                 , bench "E10" $ nf (put (evalL expr) (envn 10)) (VNum 65536)
                 , bench "E20" $ nf (put (evalL expr) (envn 20)) (VNum 65536)
                 , bench "E30" $ nf (put (evalL expr) (envn 30)) (VNum 65536)
     ]
  ]

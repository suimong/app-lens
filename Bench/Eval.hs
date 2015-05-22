{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Data.ApplicativeLens
import Examples.Evaluator hiding (incL)

import Criterion.Main

import Control.DeepSeq

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


expr1 = twice @@ twice @@ (twice @@ twice @@ twice @@ (twice @@ twice @@ twice @@ twice @@ inc)) @@ x 
    where
      twice = EFun "f" $ EFun "x" $
                EVar "f"@@ (EVar "f" @@ EVar "x")
      inc   = EFun "x" (EInc (EVar "x"))
      x     = EVar "x"

incL :: Lens Int Int
incL = lens' $ \s -> (s + 1, (\v -> v - 1))

main = defaultMain [
  bgroup "evalL" [ bench "E0"    $ nf (put (evalL expr) env0)        (VNum 0)
                 , bench "E1000" $ nf (put (evalL expr) (envn 1000)) (VNum 0)
                 , bench "E2000" $ nf (put (evalL expr) (envn 2000)) (VNum 0)
                 , bench "E3000" $ nf (put (evalL expr) (envn 3000)) (VNum 0)
     ]
  ]


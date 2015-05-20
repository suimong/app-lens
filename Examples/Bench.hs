{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- module Examples.Bench where

import Data.ApplicativeLens
import Examples.Evaluator 

import Control.DeepSeq

-- import Criterion.Main

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


test n = unlift (\x -> iterate (lift incL) x !! n)
test2 = unliftT (\(x:xs) -> foldl (lift2 addL) x xs)
  where
    addL :: Lens (Int, Int) Int 
    addL = lens' $ \(a,b) -> (a + b, \v -> (v - b, b))
    

-- main = do print $ (put (evalL expr1) env0) (VNum 0)
--           print $ rnf $ (put (evalL expr1) (envn 1000)) (VNum 0)

main = do print $ put (test 10000000) 0 0

-- main = do print $ let xs = put test2 [0..10000] 0
--                   in xs `deepseq` take 10 xs


{-
main = defaultMain [
  bgroup "evalL" [ bench "E0"  $ nf (put (evalL expr) env0)      (VNum 65536)
                 , bench "E1000" $ nf (put (evalL expr) (envn 1000)) (VNum 65536)
                 , bench "E2000" $ nf (put (evalL expr) (envn 2000)) (VNum 65536)
                 , bench "E3000" $ nf (put (evalL expr) (envn 3000)) (VNum 65536)
     ]
  ]
-}

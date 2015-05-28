{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Control.LensFunction
import Examples.Evaluator hiding (incL)

import Criterion.Main

test n = unlift (\x -> iterate (lift incL) x !! n)

test2 = unliftT (\(x:xs) -> foldl (lift2 addL) x xs)
  where
    addL :: Lens (Int, Int) Int 
    addL = lens' $ \(a,b) -> (a + b, \v -> (v - b, b))
    

incL :: Lens Int Int
incL = lens' $ \s -> (s + 1, (\v -> v - 1))


main = defaultMain [
  bgroup "composition" [ bench "U10000000" $ nf (put (test 10000000) 0) 0
                       , bench "U20000000" $ nf (put (test 20000000) 0) 0
                       , bench "B5000"     $ nf (put test2 [0..5000]) 0
                       , bench "B10000"    $ nf (put test2 [0..10000]) 0
                       ]
  ]


{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Control.LensFunction
import Examples.Evaluator hiding (incL)

import Criterion.Main
import Control.Lens

test n = unlift (\x -> iterate (lift incL) x !! n)
  where
    incL :: Lens' Int Int
    incL = lens' $ \s -> (s + 1, (\v -> v - 1))
    
test2 = unliftT (\(x:xs) -> foldl (lift2 addL) x xs)
  where
    addL :: Lens' (Int, Int) Int 
    addL = lens' $ \(a,b) -> (a + b, \v -> (v - b, b))
    

put l s v = set l v s

main = putStrLn $ show $ (put (test 10000000) 0) 0

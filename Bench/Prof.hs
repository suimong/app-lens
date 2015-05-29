{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Control.LensFunction

import Criterion.Main
import Control.Lens

test n =
  unlift (\x -> iterate (lift incL) x !! n)
  where
    incL :: Lens' Int Int
    incL = lens' $ \s -> (s + 1, \v -> v - 1)
    
put l s v = set l v s

main = putStrLn $ show $ (put (test 15000000) 0) 0

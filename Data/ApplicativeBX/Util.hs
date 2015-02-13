module Data.ApplicativeBX.Util where

import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import Data.Foldable (Foldable)
import qualified Data.Foldable as F

import qualified Control.Monad.State as St 

contents :: Traversable t => t a -> [a] 
contents = F.toList

{-# SPECIALIZE contents :: [a] -> [a] #-}

fill :: Traversable t => t b -> [a] -> t a
fill t xs = St.evalState (T.traverse next t) xs
  where
    next _ = do (x:xs) <- St.get
                St.put xs
                return x
{-# SPECIALIZE fill :: [b] -> [a] -> [a] #-}
                
shape :: Functor f => f a -> f ()
shape t = fmap (\_ -> ()) t 

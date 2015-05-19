module Data.ApplicativeLens.Util where

import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import Data.Foldable (Foldable)
import qualified Data.Foldable as F

import qualified Control.Monad.State as St 

contents :: Traversable t => t a -> [a] 
contents = F.toList

fill :: Traversable t => t b -> [a] -> t a
fill t xs = St.evalState (T.traverse next t) xs
  where
    next _ = do (x:xs) <- St.get
                St.put xs
                return x

{-# INLINE[2] fill #-}
{-# INLINE[2] contents #-}                
{-# RULES
"fill/list"     fill = fillList
"contents/list" contents = (id :: [a] -> [a])
  #-}
fillList xs ys | length xs == length ys = ys

shape :: Functor f => f a -> f ()
shape t = fmap (\_ -> ()) t 

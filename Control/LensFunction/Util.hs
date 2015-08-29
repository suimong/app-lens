{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.LensFunction.Util where

import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import qualified Data.Foldable as F

import qualified Control.Monad.State as St 

contents :: Traversable t => t a -> [a] 
contents = F.toList

fill :: Traversable t => t b -> [a] -> t a
fill t = St.evalState (T.traverse next t)
  where
    next _ = do (y:ys) <- St.get
                St.put ys
                return y

{-# INLINABLE[2] fill #-}
{-# INLINABLE[2] contents #-}                
{-# RULES
"fill/list"     fill = fillList
"contents/list" contents = id :: [a] -> [a]
  #-}
fillList :: [a] -> [b] -> [b]
fillList _ ys = ys

shape :: Functor f => f a -> f ()
shape = fmap (const ()) 

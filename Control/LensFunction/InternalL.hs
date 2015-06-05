{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}

{- | Lens in the van Laarhoven representation -}

module Control.LensFunction.InternalL where

import Control.Arrow (first, second)
import Control.Applicative (Const(..))
import Control.Monad.Identity 

import qualified Control.Lens as L 


type LensI s v = L.Lens' s v

fromLens :: L.Lens' s v -> LensI s v 
fromLens x = x
{-# INLINE[2] fromLens #-}

toLens :: LensI s v -> L.Lens' s v
toLens x = x
{-# INLINE[2] toLens #-}

get :: LensI s v -> s -> v 
get l = L.view l -- the argument is necessary to pass the type check.

{-# INLINE get #-}

put :: LensI s v -> s -> v -> s
put lens = flip (L.set lens) 
{-# INLINE put #-}

newtype Store a b = Store { runStore :: (a, a -> b) }

instance Functor (Store a) where
  {-# INLINE fmap #-}
  fmap f (Store (a, r)) = Store (a, f . r)

viewrefl :: LensI s v -> (s -> (v, v -> s))
viewrefl lens s = runStore $ lens (\v -> Store (v, id)) s  
{-# INLINE viewrefl #-}


lensI :: (s -> v) -> (s -> v -> s) -> LensI s v
lensI = L.lens
{-# INLINE lensI #-}

lensI' :: (s -> (v, v -> s)) -> LensI s v
lensI' h = \f s -> let (v,r) = h s
                       in fmap r (f v)
{-# INLINE lensI' #-}

(***) :: LensI a b -> LensI a' b' -> LensI (a,a') (b,b')
x *** y = L.alongside x y
{-# INLINE (***) #-}

(<<<) = flip (.)
{-# INLINE (<<<) #-}

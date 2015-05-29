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
{-# INLINABLE[2] fromLens #-}

toLens :: LensI s v -> L.Lens' s v
toLens x = x
{-# INLINABLE[2] toLens #-}

get :: LensI s v -> s -> v 
get lens = L.view lens

{-# INLINABLE get #-}

put :: LensI s v -> s -> v -> s
put lens = flip (L.set lens) 
{-# INLINABLE put #-}


modify :: LensI s v -> (v -> v) -> s -> s
modify lens = L.over lens
{-# INLINABLE modify #-}

newtype Store a b = Store { runStore :: (a, a -> b) }

instance Functor (Store a) where
  {-# INLINE fmap #-}
  fmap f (Store (a, r)) = Store (a, f . r)

viewrefl :: LensI s v -> (s -> (v, v -> s))
viewrefl lens s = runStore $ lens (\v -> Store (v, id)) s  
{-# INLINABLE viewrefl #-}


lensI :: (s -> v) -> (s -> v -> s) -> LensI s v
lensI = L.lens
{-# INLINABLE lensI #-}

lensI' :: (s -> (v, v -> s)) -> LensI s v
lensI' h = \f -> \s -> let (v,r) = h s
                       in fmap r (f v)
{-# INLINABLE lensI' #-}

(***) :: LensI a b -> LensI a' b' -> LensI (a,a') (b,b')
x *** y = L.alongside x y
{-# INLINABLE (***) #-}

(<<<) = flip (.)
{-# INLINABLE (<<<) #-}

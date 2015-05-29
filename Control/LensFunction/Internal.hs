{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns #-}

{- | Internal Representation of Lenses -}

module Control.LensFunction.Internal
       (
         LensI(), get, put
       , lensI, lensI', viewrefl
       , fromLens
       , toLens 
       , (***), (<<<)
       ) where

import qualified Control.Lens as L 

#ifdef __USE_VAN_LAARHOVEN__
import Control.LensFunction.InternalL
#else
import Control.Applicative
#endif 

#ifndef __USE_VAN_LAARHOVEN__

newtype Store v s = Store (v, v -> s)

instance Functor (Store v) where
  {-# INLINE[2] fmap #-}
  fmap f (Store (v, g)) = Store (v, f.g)

fromLens :: L.Lens' s v -> LensI s v
fromLens l = LensI $ \s -> let Store vr = f s
                           in vr
  where
    f = {-# SCC "fmap_fromLens" #-} l (\v -> Store (v,id))

toLens :: LensI s v -> L.Lens' s v
toLens (LensI f) = \u s -> let (v, r) = f s
                           in fmap r (u v)

{-# INLINABLE[2] fromLens #-}
{-# INLINABLE[2] toLens #-}

{- |
A variant of conventional representation. 
-}
newtype LensI s v = LensI { runLens :: s -> (v, v -> s) }

get :: LensI s v -> s -> v
get lens = fst . runLens lens
{-# INLINABLE get #-}

put :: LensI s v -> s -> v -> s
put lens = snd . runLens lens
{-# INLINABLE put #-}

modify :: LensI s v -> (v -> v) -> s -> s
modify lens f s =
  let (v, r) = runLens lens s
  in r (f v)
{-# INLINABLE modify #-}     

lensI :: (s -> v) -> (s -> v -> s) -> LensI s v
lensI g p = LensI (\s -> (g s, \v -> p s v))
{-# INLINABLE lensI #-}

lensI' :: (s -> (v, v -> s)) -> LensI s v
lensI' h = LensI h
{-# INLINABLE lensI' #-}

viewrefl l = \s -> runLens l s
{-# INLINABLE viewrefl #-}

(<<<) :: LensI b c -> LensI a b -> LensI a c 
y <<< x = LensI $ \s ->
                  let !(v1, r1) = runLens x s
                      !(v2, r2) = runLens y v1
                  in (v2, r1 . r2)
{-# INLINABLE (<<<) #-}

(***) :: LensI a s -> LensI b t -> LensI (a,b) (s,t)
x *** y = LensI $ \(a,b) ->
                  let !(va, ra) = runLens x a
                      !(vb, rb) = runLens y b
                  in ((va,vb), \(va',vb') -> (ra va', rb vb'))

{-# INLINABLE (***) #-}




#endif 


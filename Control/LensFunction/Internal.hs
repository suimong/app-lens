{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
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
  {-# INLINE fmap #-}
  fmap = storeMap

storeMap f (Store (v, !g)) = Store (v, f . g)
{-# INLINE storeMap #-}

fromLens :: L.Lens' s v -> LensI s v
fromLens l = fromLens' l

fromLens' :: ((v -> Store v v) -> (s -> Store v s)) -> LensI s v 
fromLens' l = 
  -- lensI (getConst . l Const) (\s v -> L.runIdentity $ l (\_ -> L.Identity v) s)
  let f = l (\v -> Store (v,id))
  in LensI $ \s -> let Store !vr = f s
                   in vr
{-# INLINE fromLens' #-}                      

toLens :: LensI s v -> L.Lens' s v
toLens (LensI f) = \u s -> let (v, r) = f s
                           in fmap r (u v)

{-# INLINE[0] fromLens #-}
{-# INLINE[0] toLens #-}

{-# RULES
"SPECIALIZE fromLens" forall (x :: L.Lens' s v). fromLens x = fromLens' (x :: (v -> Store v v) -> (s -> Store v s))
  #-}

{- |
A variant of conventional representation. 
-}
newtype LensI s v = LensI { runLens :: s -> (v, v -> s) }

get :: LensI s v -> s -> v
get lens = fst . runLens lens
{-# INLINE get #-}

put :: LensI s v -> s -> v -> s
put lens = snd . runLens lens
{-# INLINE put #-}

modify :: LensI s v -> (v -> v) -> s -> s
modify lens f s =
  let (v, r) = runLens lens s
  in r (f v)
{-# INLINE modify #-}     

lensI :: (s -> v) -> (s -> v -> s) -> LensI s v
lensI g p = LensI (\s -> (g s, \v -> p s v))
{-# INLINE lensI #-}

lensI' :: (s -> (v, v -> s)) -> LensI s v
lensI' h = LensI h
{-# INLINE lensI' #-}

viewrefl = runLens
{-# INLINE viewrefl #-}

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


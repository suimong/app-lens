{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}

{- | Internal Representation of Lenses -}

module Control.LensFunction.Internal
       (
         LensI(), get, put 
       , lensI, lensI', viewrefl
       , fromLens
       , toLens 
       , (***), (<<<)
       ) where

#ifdef __USE_VAN_LAARHOVEN__

import Control.LensFunction.InternalL

#else

import qualified Control.Lens as L 
import Control.Applicative

newtype Store v s = Store (StrictPair v (v -> s))

instance Functor (Store v) where
  fmap f (Store (StrictPair v g)) =
    Store $ StrictPair v (f.g)


fromLens :: L.Lens' s v -> LensI s v
fromLens l = LensI $ \s -> let Store vr = l (\v -> Store $ StrictPair v id) s
                           in vr

toLens :: LensI s v -> L.Lens' s v
toLens (LensI f) = \u s -> let StrictPair v r = f s
                           in fmap r (u v)

data StrictPair a b = StrictPair !a !b 

fstS (StrictPair a _) = a
sndS (StrictPair _ b) = b

{- |
A variant of conventional representation. 
-}
newtype LensI s v = LensI { runLens :: s -> StrictPair v (v -> s) }

get :: LensI s v -> s -> v
get lens = fstS . runLens lens
{-# INLINABLE get #-}

put :: LensI s v -> s -> v -> s
put lens = sndS . runLens lens
{-# INLINABLE put #-}

modify :: LensI s v -> (v -> v) -> s -> s
modify lens f s =
  let StrictPair v r = runLens lens s
  in r (f v)
{-# INLINABLE modify #-}     

lensI :: (s -> v) -> (s -> v -> s) -> LensI s v
lensI g p = LensI (\s -> StrictPair (g s) (\v -> p s v))
{-# INLINABLE lensI #-}

lensI' :: (s -> (v, v -> s)) -> LensI s v
lensI' h = LensI $ uncurry StrictPair . h
{-# INLINABLE lensI' #-}

viewrefl l = \s -> let StrictPair a b = runLens l s in (a,b)
{-# INLINABLE viewrefl #-}

(<<<) :: LensI b c -> LensI a b -> LensI a c 
y <<< x = LensI $ \s ->
                  let StrictPair v1 r1 = runLens x s
                      StrictPair v2 r2 = runLens y v1
                  in StrictPair v2 (r1 . r2)
{-# INLINABLE (<<<) #-}

(***) :: LensI a s -> LensI b t -> LensI (a,b) (s,t)
x *** y = LensI $ \(a,b) ->
                  let StrictPair va ra = runLens x a
                      StrictPair vb rb = runLens y b
                  in StrictPair (va,vb) (\(va',vb') -> (ra va', rb vb'))

{-# INLINABLE (***) #-}

#endif 


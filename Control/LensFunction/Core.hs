{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
-- Required for unliftM, unliftM2, unliftMT, if we use var Laarhoven repl.
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE Safe #-}

module Control.LensFunction.Core where
    
import Prelude -- hiding ((.), id, sequence) 
-- import Control.Category

import Data.Traversable (Traversable)
import Control.Applicative (Applicative, pure, (<*>))
import Control.Monad (ap)

import Control.LensFunction.Util
import Control.LensFunction.Exception
import Control.LensFunction.Internal

import qualified Control.Lens as L 

import qualified Data.IntMap as IM

import Data.Maybe (fromJust) 
import Control.Exception 

import qualified Control.Monad.State as St 


dup :: Poset s => LensI s (s,s)
dup = lensI' $ \s -> ((s,s), \(t,t') -> lub t t')
{-# INLINE dup #-}


{- | A type class for partially ordered sets, in which pairs are
compaired componentwisely. Instead of having partial comparison
operator, it provides an associative, commutative and idempotent
operator, which can be partial like "join" in semilattice but can be
partial.
-}          
class Poset s where
    lub :: s -> s -> s -- ^ Operation to take LUB 
    
data Tag a = O { unTag :: a } -- Original, or irrelevant
           | U { unTag :: a } -- Updated, or relevant 

instance Eq a => Poset (Tag a) where
  lub (O a) (O b) | a == b = O a
  lub (O a) (U b)          = U b
  lub (U a) (O b)          = U a
  lub (U a) (U b) | a == b = U a

instance (Poset a, Poset b) => Poset (a,b) where
  lub (a,b) (a',b') = (lub a a', lub b b')

instance (Poset a, Eq (t ()), Traversable t) => Poset (t a) where
  {-# SPECIALIZE lub :: Poset a => [a] -> [a] -> [a] #-}
  lub t1 t2 = if shape t1 == shape t2 then
                fill t1 (zipWith lub (contents t1) (contents t2))
              else
                throw (NoLUBException "Control.LensFunction.lub")
  
-- Internally used datastructure for a slightly first merge
data Diff t a = Diff (t ())              -- Shape of the data -- Assumption: t is Traversable
                     (IM.IntMap a)       -- Original mapping from indices to values 
                     (IM.IntMap (Tag a)) -- Updated mapping 

{-# SPECIALIZE toDiff :: [a] -> Diff [] a #-}
toDiff :: Traversable t => t a -> Diff t a
toDiff s = let om = IM.fromAscList $ zip [0..] (contents s)
           in Diff (shape s) om (IM.empty)


{-# SPECIALIZE fromDiff :: Diff [] a -> [a] #-}
fromDiff :: Traversable t => Diff t a -> t a
fromDiff (Diff sh om um) =
  let cs = map (\i -> case IM.lookup i um of
                       Just v  -> unTag v
                       Nothing -> fromJust $ IM.lookup i om) [0..]
  in fill sh cs 

instance Eq a => Poset (Diff t a) where
  lub (Diff t1 o1 m1) (Diff t2 o2 m2) -- Invariant: t1 == t2 and o1 == o2 
    = Diff t1 o1 (IM.unionWith lub m1 m2)



{- |
An abstract type for "updatable" data. 
-}
newtype L s a = L { unL :: Poset s => LensI s a }


{- |
The lifting function. Note that it defines a functor from the
category of lenses to the category of sets and functions.
-}
lift :: L.Lens' a b -> (forall s. L s a -> L s b)
lift l (L x) = L (fromLens l <<< x)

{-# INLINABLE lift #-}

{- | A paring function of @L s a@-typed values. -}
pair :: L s a -> L s b -> L s (a,b) 
pair (L x) (L y) = L ((x *** y) <<< dup)

{-# INLINABLE pair #-}

-- | An alternative notation. 
infixr 5 >*<
(>*<) = pair

unit :: L s ()
unit = L $ lensI' (\s -> ( (), \() -> s ) )



{- | The unlifting function, satisfying @unlift (lift x) = x@. -}
unlift :: Eq a => (forall s. L s a -> L s b) -> L.Lens' a b
unlift f = toLens $ unL (f id') <<< tag

id' :: L (Tag s) s 
id' = L $ lensI unTag (const U)
{-# INLINE id' #-}

tag :: LensI s (Tag s)
tag = lensI O (const unTag)
{-# INLINE tag #-}

{- | The unlifting function for binary functions, satisfying
     @unlift2 (lift2 x) = x@ where @lift2 l x y = lift l (pair x y)@  -}
unlift2 :: (Eq a, Eq b) => (forall s. L s a -> L s b -> L s c) -> L.Lens' (a,b) c
unlift2 f = toLens $ unL (f fst' snd') <<< tag2


fst' :: L (Tag a,b) a
fst' = L $ lensI (unTag . fst) (\(_,b) a -> (U a, b))
{-# INLINE fst' #-}

snd' :: L (a, Tag b) b 
snd' = L $ lensI (unTag . snd) (\(a,_) b -> (a, U b))
{-# INLINE snd' #-}

tag2 :: LensI (a,b) (Tag a, Tag b)
tag2 = lensI (\(a,b) -> (O a, O b)) (\_ (a,b) -> (unTag a, unTag b))
{-# INLINE tag2 #-}


unliftT :: (Eq a, Eq (t ()), Traversable t) =>
           (forall s. t (L s a) -> L s b) -> L.Lens' (t a) b
unliftT f = toLens $ 
  lensI' $ \s -> let l = makeLens s
                 in viewrefl l s 
  where
--    makeLens s = unL (f (projs (shape s))) <<< tagT
    makeLens s = unL (f (projsV (shape s))) <<< diffL 

tagT :: Functor f => LensI (f s) (f (Tag s))
tagT = lensI (fmap O) (\_ -> fmap unTag)
{-# INLINE tagT #-}

diffL = lensI' $ \s -> (toDiff s, \v -> fromDiff v)
{-# INLINE diffL #-}


-- V is from Voigtlaender    
projsV :: Traversable t => t b -> t (L (Diff t a) a)
projsV sh =
  let n = length (contents sh)
  in fill sh $ map (projV sh) [0..n-1]

projV :: Traversable t => t b -> Int -> L (Diff t a) a
projV sh i = L $ lensI' $ \(Diff s o m) ->
                           ( fromJust (IM.lookup i o),
                             \v -> Diff s o (IM.singleton i (U v)))
                          
  
     

projs :: Traversable t => t b -> t (L (t (Tag a)) a)
projs sh =
  let n = length (contents sh)
  in fill sh $ map (proj sh) [0..n-1] 


proj :: Traversable t => t b -> Int -> L (t (Tag a)) a
proj sh i = L $
            lensI  (\s -> unTag (contents s !! i))
                   (\s v -> fill sh (update i (U v) (contents s)))

update 0 v (_:xs) = v:xs
update i v (x:xs) = x:update (i-1) v xs 


-- TODO: TH Generator for lifting and unlifting functions. 


---------------------------------------

-- | An abstract monad used to keep track of observations. 
newtype R s a = R { unR :: Poset s => s -> (a, s -> Bool) }

instance Functor (R s) where
  fmap f (R m) = R $ \s -> let (x, p) = m s in (f x, p)

instance Monad (R s) where
  return x = R $ \_ -> (x, \_ -> True)
  R m >>= f = R $ \s -> let (x,c1) = m s
                            (y,c2) = let R k = f x in k  s
                        in (y, \s -> c1 s && c2 s)

instance Applicative (R s) where
  pure  = return
  (<*>) = ap

      

{- |
A premitive used to define 'liftO' and 'liftO2'. 
-}
observe :: Eq w => L s w -> R s w
observe l = R $ \s ->  let w = get (unL l) s
                       in (w, \s' -> get (unL l) s' == w)

{-# INLINABLE observe #-}

{- | A monadic version of 'unlift' -}
unliftM :: Eq a => (forall s. L s a -> R s (L s b)) -> L.Lens' a b
unliftM f = toLens $ lensI' $ \s -> viewrefl (makeLens f s) s 
  where
    makeLens f s =
      let (l,p) = unR (f id') (O s)
          l'    = unL l <<< tag
          put' s v =
            let s' = put l' s v
            in if p (O s') then
                 s'
               else
                 throw (ChangedObservationException "Control.Lens.Function.unliftM")
      in lensI (get l')  put'

{- | A monadic version of 'unlift2' -}
unliftM2 :: (Eq a, Eq b) =>
            (forall s. L s a -> L s b -> R s (L s c)) -> L.Lens' (a,b) c
unliftM2 f = toLens $ lensI' $ \s -> viewrefl (makeLens f s) s 
  where
    makeLens f s =
      let (l,p) = unR (f fst' snd') (get tag2 s)
          l'    = unL l <<< tag2
          put' s v =
            let s' = put l' s v
            in if p (get tag2 s') then
                 s'
               else
                 throw (ChangedObservationException "Control.LensFunction.unliftM2")
      in lensI (get l')  put'


{- | A monadic version of 'unliftT' -}
unliftMT :: (Eq a, Eq (t ()), Traversable t) =>
            (forall s. t (L s a) -> R s (L s b)) -> L.Lens' (t a) b
unliftMT f = toLens $ lensI' $ \s -> viewrefl (makeLens f s) s
  where
    makeLens f s =
      let (l,p) = unR (f (projsV (shape s))) (get diffL s)
          l'    = unL l <<< diffL
          -- (l,p) = unR (f (projs (shape s))) (get tagT s)
          -- l'    = unL l <<< tagT
          put' s v =
            let s' = put l' s v
            in if p (get diffL {- tagT -} s') then
                 s'
               else
                 throw (ChangedObservationException "Control.LensFunction.unliftMT")
      in lensI (get l')  put'
           
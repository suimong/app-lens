{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
-- Required for unliftM, unliftM2, unliftMT, if we use var Laarhoven repl.
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE Trustworthy #-}

module Control.LensFunction.Core where
    
import Prelude -- hiding ((.), id, sequence) 
-- import Control.Category

import Data.Traversable (Traversable)
import Control.Applicative (Applicative, pure, (<*>))
import Control.Monad (ap)

import Control.LensFunction.Util
import Control.LensFunction.Exception
import Control.LensFunction.Internal

import qualified Control.Lens as L (Lens', lens)

import qualified Data.IntMap as IM

import Data.Maybe (fromJust) 
import Control.Exception 


{- | 
A variant of 'Control.Lens.lens'. Sometimes, this function would be
easier to use because one can re-use a source information to define a "put". 
-}
lens' :: (s -> (v, v -> s)) -> L.Lens' s v
lens' f = \u s -> let (v,r) = f s
                  in fmap r (u v)
{-# INLINE[1] lens' #-}

{- |
Just a composition of 'lift' and 'Control.Lens.lens'.
Sometimes, this function would be more efficient than the composition
due to eliminated conversion from the lens to the internal representation.

Since both of the internal and the external representations are
functions (= normal forms), we have to pay the conversion cost for
each time when the lifted lens function is evaluated, even in the lazy
evaluation.

We actually has the RULE to make the composition of 'lift' and
'Control.Lens.lens' to 'liftLens'. However, the rule may not be fired
especially when profiling codes are inserted by GHC.
-}
liftLens :: (a -> b) -> (a -> b -> a) -> (forall s. L s a -> L s b)
liftLens g p = liftI (lensI g p)

{- |
Just a composition of 'lift' and 'lens''. This function has the
similar role to 'liftLens'.
-}

liftLens' :: (a -> (b, b -> a)) -> (forall s. L s a -> L s b)
liftLens' f = liftI (lensI' f)



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
  lub (O _) (U b)          = U b
  lub (U a) (O _)          = U a
  lub (U a) (U b) | a == b = U a
  lub _     _              = throw (NoLUBException "Control.LensFunction.lub")

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
           in Diff (shape s) om IM.empty


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
An abstract type for "updatable" data. Bidirectional programming
through our module is to write manipulation of this datatype.



==== Categorical Notes

The type constructor @L s@ together with 'lift', 'unit' and 'pair'
defines a lax monoidal functor from the category of lenses to that of
Haskell functions. The 'lift' function does transfor a lens to a
function. The 'unit' and 'pair' functions are the core operations on
this lax monoidal functor.  Any lifting functions defined in this
module can be defined by these three functions.


-}
newtype L s a = L (Poset s => LensI s a)

unL :: L s a -> (Poset s => LensI s a)
unL (L s) = s
{-# INLINE unL #-}


{-
f id' <<< tag <<< y
vs.
f y 

lift2 id :: L s A -> L s B -> L s (A,B)

f y = pair y y


(y *** y) <<< dup 
vs
(id' *** id') <<< dup <<< tag <<< y

y = (\x -> unTag y) (\_ v -> O v) --- NOT locally well-behaved

put ((y *** y) <<< dup) s (s,v) = _|_

put ((id' *** id') <<< dup <<< tag <<< y) s (s,v) = O v

-}

{- |
The lifting function. Note that it forms a functor from the
category of lenses to the category of sets and functions.

'unlift' is a left-inverse of this function.

prop> unlift (lift x) = x
-}
{-
Notice that 'lift' is not a surjection. That is,
there is a function such that @lift (unlift f) /= f@.
However such a function cannot be constructed with `lift`.
-}
lift :: L.Lens' a b -> (forall s. L s a -> L s b)
lift l = liftI (fromLens l)

liftI :: LensI a b -> (forall s. L s a -> L s b)
liftI h = \(L x) -> L (h <<< x)

{-# NOINLINE[1]  lift #-}
{-# INLINE       liftI #-}

{- | A paring function of @L s a@-typed values.
This function can be defined by 'lift2' as below.

prop> pair = lift2 (lens id (const id)) 
-}
pair :: L s a -> L s b -> L s (a,b) 
pair (L x) (L y) = L ((x *** y) <<< dup)

{-# INLINE pair #-}


{- |
The unit element in the lifted world.

Let @elimUnitL@ and @elimUnitR@ are lenses defined as follows.

@
elimUnitL = lens (\(x,()) -> x) (\_ x -> (x,()))
elimUnitR = lens (\((),x) -> x) (\_ x -> ((),x))
@

Then, we have the following laws.

prop> lift2 elimUnitL x unit = x
prop> lift2 elimUnitR unit x = x
-}
unit :: L s ()
unit = L $ lensI' (\s -> ( (), \() -> s ) )
{-# INLINABLE unit #-}


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
     @unlift2 (lift2 x) = x@.
-}
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

{- |
The unlifting function for functions that manipulate data structures,
satisfying @unliftT (liftT x) = x@ if @x@ keeps the shape.
The constraint @Eq (t ())@ says that we can compare the shapes of
given two containers. 

-}
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

diffL :: Traversable t => LensI (t a) (Diff t a)
diffL = lensI' $ \s -> (toDiff s, fromDiff)
{-# INLINE diffL #-}


-- V is from Voigtlaender    
projsV :: Traversable t => t b -> t (L (Diff t a) a)
projsV sh =
  let n = length (contents sh)
  in fill sh $ map (projV sh) [0..n-1]

projV :: Traversable t => t b -> Int -> L (Diff t a) a
projV _ i = L $ lensI' $ \(Diff s o _) ->
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

update :: Int -> a -> [a] -> [a]
update 0 v (_:xs) = v:xs
update i v (x:xs) = x:update (i-1) v xs 
update _ _ _      = error "Invalid Index"

-- TODO: TH Generator for lifting and unlifting functions. 


---------------------------------------

{- |
An abstract monad used to keep track of observations.
By this monad, we can inspect the value of 'L s a'-data.

It is worth noting that we cannot change the inspection result to ensure
the consistency property (aka PutGet in some context).


-}
newtype R s a = R { unR :: Poset s => s -> (a, s -> Bool) }

instance Functor (R s) where
  fmap f (R m) = R $ \s -> let (x, p) = m s in (f x, p)

instance Monad (R s) where
  return x = R $ const (x, const True)
  R m >>= f = R $ \s -> let (x,c1) = m s
                            (y,c2) = let R k = f x in k s
                        in (y, \t -> c1 t && c2 t)

instance Applicative (R s) where
  pure  = return
  (<*>) = ap

      

{- |
A primitive used to define 'liftO' and 'liftO2'.
With 'observe', one can inspect the current value of a lifted '(L s a)'-value
as below.

@
f x :: L s A -> R s (L s B)
f x = do viewX <- observe x
         ... computation depending on 'viewX' ...
@

Once the 'observe' function is used in a lens function,
the lens function becomes not able to change 
change the "observed" value to ensure the correctness.

-}
observe :: Eq w => L s w -> R s w
observe l = R $ \s ->  let w = get (unL l) s
                       in (w, \s' -> get (unL l) s' == w)

{-# INLINABLE observe #-}

{- | A monadic version of 'unlift'. -}
unliftM :: Eq a => (forall s. L s a -> R s (L s b)) -> L.Lens' a b
unliftM f = toLens $ lensI' $ \src -> viewrefl (makeLens src) src 
  where
    makeLens src =
      let (l,p) = unR (f id') (O src)
          l'    = unL l <<< tag
          put' s v =
            let s' = put l' s v
            in if p (O s') then
                 s'
               else
                 throw (ChangedObservationException "Control.Lens.Function.unliftM")
      in lensI (get l')  put'

{- | A monadic version of 'unlift2'. -}
unliftM2 :: (Eq a, Eq b) =>
            (forall s. L s a -> L s b -> R s (L s c)) -> L.Lens' (a,b) c
unliftM2 f = toLens $ lensI' $ \src -> viewrefl (makeLens src) src 
  where
    makeLens src =
      let (l,p) = unR (f fst' snd') (get tag2 src)
          l'    = unL l <<< tag2
          put' s v =
            let s' = put l' s v
            in if p (get tag2 s') then
                 s'
               else
                 throw (ChangedObservationException "Control.LensFunction.unliftM2")
      in lensI (get l')  put'


{- | A monadic version of 'unliftT'. -}
unliftMT :: (Eq a, Eq (t ()), Traversable t) =>
            (forall s. t (L s a) -> R s (L s b)) -> L.Lens' (t a) b
unliftMT f = toLens $ lensI' $ \src -> viewrefl (makeLens src) src
  where
    makeLens src =
      let (l,p) = unR (f (projsV (shape src))) (get diffL src)
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
           
{-# RULES
"lift/lens'"       forall x.   lift (lens' x)        = liftI (lensI' x)
"lift/lens"        forall g p. lift (L.lens g p)     = liftI (lensI g p)
"lens/fromLens"    forall g p. fromLens (L.lens g p) = lensI g p
"lens'/fromLens"   forall f.   fromLens (lens' f)    = lensI' f
  #-}


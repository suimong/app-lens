{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Data.ApplicativeBX.Core
    (
      Lens(..)
    , L() -- abstract
    , unit, pair 
    , lift
    , unlift, unlift2, unliftT
    , enforce, observe 
    ) where
    
import Prelude hiding ((.), id, sequence) 
import Control.Category

import Data.Traversable (Traversable)
import Data.ApplicativeBX.Util
import Control.Applicative (Applicative, pure, (<*>))
import Control.Monad (ap)

import qualified Control.Monad.State as St 

{- | 
The conventional representation of lenses. Note that the identical
represention of @s -> (v, v -> s)@ is faster but we adopt this
reprsentation for simplicity.
-}
data Lens s v = Lens { get :: s -> v,  put :: s -> v -> s }



instance Category Lens where
    id = Lens id (const id)

    Lens g2 p2 . Lens g1 p1 =
        Lens (g2 . g1) (\s -> p1 s . p2 (g1 s))

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
  lub t1 t2 = if shape t1 == shape t2 then
                fill t1 (zipWith lub (contents t1) (contents t2))
              else
                error "No LUB"
                

{- | An abstract type for "updatable" data. 
-}
newtype L s a = L { unL :: Poset s => Lens s a }

{- | The lifting function. Note that it defines a functor from the
category of lenses to the category of sets and functions.
-}
lift :: Lens a b -> (forall s. L s a -> L s b)
lift lens (L x) = L (lens . x)

dup :: Poset s => Lens s (s,s)
dup = Lens (\s -> (s,s)) (\v (s,t) -> lub s t)

(***) :: Lens a b -> Lens a' b' -> Lens (a,a') (b,b')
x *** y = Lens (\(a,a') -> (get x a, get y a'))
               (\(a,a') (b,b') ->
                 (put x a b, put y a' b'))

{- | A paring function of @L s a@-typed values. -}
pair :: L s a -> L s b -> L s (a,b) 
pair (L x) (L y) = L ((x *** y) . dup)

-- | An alternative notation. 
infixr 5 >*<
(>*<) = pair

unit :: L s ()
unit = L $ Lens (\_ -> ()) (\s () -> s)
  
{- | The unlifting function, satisfying @unlift (lift x) = x@. -}
unlift :: Eq a => (forall s. L s a -> L s b) -> Lens a b
unlift f = unL (f id') . tag
  where
    id' = L $ Lens unTag (const U)
    tag = Lens O (const unTag)

{- | The unlifting function for binary functions, satisfying
     @unlift2 (lift2 x) = x@ where @lift2 l x y = lift l (pair x y)@  -}
unlift2 :: (Eq a, Eq b) => (forall s. L s a -> L s b -> L s c) -> Lens (a,b) c
unlift2 f = unL (f fst' snd') . tag2
  where
    fst' = L $ Lens (unTag . fst) (\(_,b) a -> (U a, b))
    snd' = L $ Lens (unTag . snd) (\(a,_) b -> (a, U b))

    tag2 = Lens (\(a,b) -> (O a, O b)) (\_ (a,b) -> (unTag a, unTag b))


unliftT :: (Eq a, Eq (t ()), Traversable t) =>
           (forall s. t (L s a) -> L s b) -> Lens (t a) b
unliftT f = Lens (\s -> get (mkLens s) s)
                 (\s -> put (mkLens s) s)
  where
    mkLens s = unL (f (projs (shape s))) . tagT 
    tagT = Lens (fmap O) (\_ -> fmap unTag)
           
    projs sh =
      let n = length (contents sh)
      in fill sh $ map (proj sh) [0..n-1] 
    proj sh i = L $
      Lens (\s -> unTag (contents s !! i))
           (\s v -> fill sh (update i (U v) (contents s)))

update 0 v (_:xs) = v:xs
update i v (x:xs) = x:update (i-1) v xs 

      

-- TODO: TH Generator for lifting and unlifting functions. 


---------------------------------------

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


observe :: Eq w => L s w -> R s w
observe l = R $ \s ->  let w = get (unL l) s
                       in (w, \s' -> get (unL l) s' == w)

enforce :: R s (L s a) -> L s a
enforce m = L $ Lens (\s -> get (mkLens m s) s)
                     (\s -> put (mkLens m s) s)
  where
    mkLens (R m) s =
      let (l,p) = m s
          put' s v = let s' = put (unL l) s v
                     in if p s' then
                          s'
                        else 
                          error "Changing Observation"
      in Lens (get (unL l)) put' 
                          

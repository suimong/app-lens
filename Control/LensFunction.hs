{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

-- Required for sequenceL, if we use var Laarhoven repl.
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Safe #-}

module Control.LensFunction
       (
         -- from Data.ApplicativeLens.Core 
         L() -- abstract
       , lens'
       , unit, pair 
       , lift
       , unlift, unlift2, unliftT
       , R() -- abstract
       , observe
       , unliftM, unliftM2, unliftMT

         -- from this module 
       , sequenceL, list 
       , new, lift2, liftT
       , liftC, liftC2 
       , liftO, liftO2
       , module Control.LensFunction.Exception
       ) where

import Control.LensFunction.Core
import Control.LensFunction.Internal (lens')
import Control.LensFunction.Util
import Control.LensFunction.Exception

import Data.Traversable (Traversable)

import Control.Exception

import qualified Control.Lens as L 
---------------------------------------------------------

mName = "Control.LensFunction"

new :: Eq a => a -> L s a
new a = lift (L.lens (const a) (\_ a' -> check a a')) unit
  where
    check a a' = if a == a' then
                   ()
                 else
                   throw (ConstantUpdateException $ mName ++ ".new")

{- | The lifting function for binary lenses -}
lift2 :: L.Lens' (a,b) c -> (L s a -> L s b -> L s c)
lift2 l x y = lift l (pair x y) 

{- Derived Functions -}

{- | Similar to @pair@, but this function is for lists. -}
list :: [L s a] -> L s [a]
list []     = lift (L.lens (\() -> [])
                           (\() -> \case { [] -> () ; _ -> throw (ShapeMismatchException $ mName ++ ".list")} ))
              unit
list (x:xs) = lift consL (pair x (list xs))
  where
    consL = L.lens (\(x,xs) -> (x:xs))
                   (\_ -> \case { (x:xs) -> (x,xs); _ -> throw (ShapeMismatchException $ mName ++ ".list") })

{- | A data-type generic version of 'list'. -}
sequenceL :: (Eq (t ()), Traversable t) => t (L s a) -> L s (t a)
sequenceL t = lift (fillL t) $ list (contents t)
  where
    fillL t = L.lens (\s -> fill t s)
                     (\_ v -> if shape t == shape v then
                                contents v
                              else
                                throw (ShapeMismatchException $ mName ++ ".sequenceL"))

{-# SPECIALIZE sequenceL :: [L s a] -> L s [a] #-}              

liftC :: Eq a => (L.Lens' a b -> L.Lens' c d) ->
             (forall s. L s a -> L s b) ->
             (forall s. L s c -> L s d)
liftC c f = lift (c (unlift f))

liftC2 :: (Eq a, Eq c) => (L.Lens' a b -> L.Lens' c d -> L.Lens' e f) 
          -> (forall s. L s a -> L s b) 
          -> (forall s. L s c -> L s d)
          -> (forall s. L s e -> L s f)
liftC2 c f g = lift (c (unlift f) (unlift g))

----------------------------------------------------------
{- | A datatype-generic version of 'lift2'-}
liftT :: (Eq (t ()), Traversable t)
         => L.Lens' (t a) b -> (forall s. t (L s a) -> L s b)
liftT l xs = lift l (sequenceL xs)

{- | Lifting of observations -}
liftO :: Eq w => (a -> w) -> L s a -> R s w
liftO p x = observe (lift (L.lens p unused) x)
  where
    unused s v | v == p s = s

{- | Lifting of binary observations -}
liftO2 :: Eq w => (a -> b -> w) -> L s a -> L s b -> R s w
liftO2 p x y = liftO (uncurry p) (x `pair` y) 

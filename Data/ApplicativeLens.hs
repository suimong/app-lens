{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

-- Required for sequenceL, if we use var Laarhoven repl.
{-# LANGUAGE NoMonomorphismRestriction #-}


module Data.ApplicativeLens
       ( module Data.ApplicativeLens.Core
       , sequenceL, list 
       , new, lift2, liftT
       , liftO, liftO2
       ) where

import Data.ApplicativeLens.Core

import Data.ApplicativeLens.Util
import Data.Traversable (Traversable)

import Control.Exception

---------------------------------------------------------

new :: Eq a => a -> L s a
new a = lift (lens (const a) (\_ a' -> check a a')) unit
  where
    check a a' = if a == a' then
                   ()
                 else
                   throw ConstantUpdateException

{- | The lifting function for binary lenses -}
lift2 :: Lens (a,b) c -> (L s a -> L s b -> L s c)
lift2 l x y = lift l (pair x y) 

{- Derived Functions -}

{- | Similar to @pair@, but this function is for lists. -}
list :: [L s a] -> L s [a]
list []     = lift (lens (\() -> []) (\() -> \case { [] -> () ; _ -> throw ShapeMismatchException} )) unit
list (x:xs) = lift consL (pair x (list xs))
  where
    consL = lens (\(x,xs) -> (x:xs))
                 (\_ -> \case { (x:xs) -> (x,xs); _ -> throw ShapeMismatchException })

{- | A data-type generic version of 'list'. -}
sequenceL :: (Eq (t ()), Traversable t) => t (L s a) -> L s (t a)
sequenceL t = lift (fillL t) $ list (contents t)
  where
    fillL t = lens (\s -> fill t s)
                   (\_ v -> if shape t == shape v then
                              contents v
                            else
                              throw ShapeMismatchException)

{-# SPECIALIZE sequenceL :: [L s a] -> L s [a] #-}              

----------------------------------------------------------
{- | A datatype-generic version of 'lift2'-}
liftT :: (Eq (t ()), Traversable t)
         => Lens (t a) b -> (forall s. t (L s a) -> L s b)
liftT l xs = lift l (sequenceL xs)

{- | Lifting of observations -}
liftO :: Eq w => (a -> w) -> L s a -> R s w
liftO p x = observe (lift (lens p unused) x)
  where
    unused s v | v == p s = s

{- | Lifting of binary observations -}
liftO2 :: Eq w => (a -> b -> w) -> L s a -> L s b -> R s w
liftO2 p x y = liftO (uncurry p) (x `pair` y) 

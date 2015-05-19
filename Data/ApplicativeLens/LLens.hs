{-# LANGUAGE RankNTypes #-}

{- | Lens in the van Laarhoven representation -}

module Data.ApplicativeLens.LLens where

import Control.Arrow (first, second)
import Control.Applicative (Const(..))
import Control.Monad.Identity 


-- var Laarhoven's represetation 
type Lens s v = forall f. Functor f => (v -> f v) -> (s -> f s)

get :: Lens s v -> s -> v 
get lens = getConst . lens (\v -> Const v)

put :: Lens s v -> s -> v -> s
put lens s v = modify lens (\_ -> v) s 

modify :: Lens s v -> (v -> v) -> (s -> s)
modify lens f s = runIdentity (lens (\v -> Identity (f v)) s)

newtype Store a b = Store { runStore :: (a, a -> b) }

instance Functor (Store a) where
  fmap f (Store (a, r)) = Store (a, f . r)

viewrefl :: Lens s v -> (s -> (v, v -> s))
viewrefl lens s = runStore $ lens (\v -> Store (v, id)) s  

lens :: (s -> v) -> (s -> v -> s) -> Lens s v
lens g p = \f -> \s -> fmap (p s) $ f (g s)

lens' :: (s -> (v, v -> s)) -> Lens s v
lens' h = \f -> \s -> let (v,r) = h s
                      in fmap r (f v)

newtype FocusFst f b a = FocusFst { runFocusFst :: f (a, b) }

instance Functor f => Functor (FocusFst f b) where
  fmap f (FocusFst xy) = FocusFst $ fmap (first f) xy

newtype FocusSnd f a b = FocusSnd { runFocusSnd :: f (a, b) } 

instance Functor f => Functor (FocusSnd f a) where
  fmap f (FocusSnd xy) = FocusSnd $ fmap (second f) xy


(***) :: Lens a b -> Lens a' b' -> Lens (a,a') (b,b')
x *** y = \f (a,a') -> 
  runFocusFst $ x (\b -> FocusFst $ runFocusSnd $
                         y (\b' -> FocusSnd $ f (b,b')) a') a
  
-- {-
--     lens (\(a,a') -> (get x a, get y a'))
--          (\(a,a') (b,b') ->
--             (put x a b, put y a' b'))
-- -}
--   \f (a,a') ->
--    let (viewx, reflx) = viewrefl x a
--        (viewy, refly) = viewrefl y a'
--    in fmap (\(b,b') -> (reflx b, refly b')) $ 
--        f (viewx, viewy)


(<<<) = flip (.)
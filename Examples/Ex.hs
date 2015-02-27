{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

{- |
This file contains the examples in the paper. 
-}

module Examples.Ex where

import Data.ApplicativeBX
import Data.List (elemIndices, splitAt)

import Data.Traversable (Traversable)
import Data.Foldable    (Foldable) 


-- Explict type declation is needed unless NoMonomorphismRestriction is on. 
tailL = unliftT tailH


tailH xs = list (tail xs)

{-
*Examples.Ex> put tailL "abc" "Ca"
"aCa"
*Examples.Ex> get tailL "abc"
"bc"
*Examples.Ex> put tailL "abc" "BC"
"aBC"
*Examples.Ex> put tailL "abc" "BCd"
"*** Exception: Data/ApplicativeBX.hs:28:39-50: Non-exhaustive patterns in lambda

*Examples.Ex> put tailL "abc" "B"
"*** Exception: Data/ApplicativeBX.hs:32:19-37: Non-exhaustive patterns in lambda
-}


unlinesH :: [L s String] -> L s String
unlinesH []     = new ""
unlinesH (x:xs) = catLineH x (unlinesH xs)
  where catLineH = lift2 catLineL

catLineL =
  Lens (\(x,y) -> (x ++ "\n" ++ y))
       (\ (x,y) v -> let n = length (filter (== '\n') x)
                         i = elemIndices '\n' v !! n 
                         (x',y') = splitAt i v 
                     in (x', tail y'))

unlinesL = unliftT unlinesH 

{-
*Examples.Ex> get unlinesL ["banana", "orange", "apple"]
"banana\norange\napple\n"
*Examples.Ex> put unlinesL ["banana", "orange", "apple"] "Banana\nOrange\nApple\n"
["Banana","Orange","Apple"]
*Examples.Ex> put unlinesL ["banana", "orange", "apple"] "Banana\nOrange\nApple"
*** Exception: Prelude.(!!): index too large

*Examples.Ex> put unlinesL ["banana", "orange", "apple"] "Banana\nOrange\nApple\n\n"
-}


-- This must be indentity-lens
mustbeID x = lift2 b x unit
  where b = Lens (\(x,()) -> x)
                 (\_ x -> (x,()))

{-
*Examples.Ex> get (unlift mustbeID) "A"
"A"
*Examples.Ex> get (unlift mustbeID) 1
1
*Examples.Ex> put (unlift mustbeID) 1 2
2
*Examples.Ex> put (unlift mustbeID) 1 323
323
*Examples.Ex> put (unlift mustbeID) "A" "B"
"B"
*Examples.Ex> put (unlift mustbeID) "Aa" "B"
"B"
-}

------------------------------------------------------

mapDefault :: a -> Lens a b -> Lens [a] [b]
mapDefault d l = Lens (map (get l)) (\s v -> go s v)
  where
    go ss [] = []
    go [] vs = go (map (const d) vs) vs
    go (s:ss) (v:vs) = put l s v : go ss vs


liftC :: Eq a => (Lens a b -> Lens c d) ->
             (forall s. L s a -> L s b) ->
             (forall s. L s c -> L s d)
liftC c f = lift (c (unlift f))


mapH :: Eq a => a -> (forall s. L s a -> L s b) -> L s [a] -> L s [b]
mapH d = liftC (mapDefault d)

mapAddL = unlift (mapH (0,0) addL)

addL = lift (Lens (uncurry (+))
            (\(x,_) v -> (x, v - x)))

mapAddL' = unliftT (list . map addL)       

{-
*Examples.Ex> get mapAddL [(1,1), (2,2)]
[2,4]
*Examples.Ex> put mapAddL [(1,1), (2,2)] [3,5]
[(1,2),(2,3)]
*Examples.Ex> put mapAddL [(1,1), (2,2)] [3]
[(1,2)]
*Examples.Ex> put mapAddL [(1,1), (2,2)] [3,5,7]
[(1,2),(2,3),(0,7)]

*Examples.Ex> get mapAddL' [(1,1), (2,2)]
[2,4]
*Examples.Ex> put mapAddL' [(1,1), (2,2)] [3,5]
[(1,2),(2,3)]
*Examples.Ex> put mapAddL' [(1,1), (2,2)] [3]
*** Exception: Data/ApplicativeBX.hs:32:19-37: Non-exhaustive patterns in lambda

*Examples.Ex> put mapAddL' [(1,1), (2,2)] [3,5,7]
*** Exception: Data/ApplicativeBX.hs:28:39-50: Non-exhaustive patterns in lambda
-}

-- toy program for observation 
good x y = fmap (uncurry pair) $ do 
  b <- liftO2 (==) x (new 0)
  return (if b then (x,y) else (x, new 1))

goodL = unliftM2 good   

{-
*Examples.Ex> get goodL (0,2)
(0,2)
*Examples.Ex> put goodL (0,2) (1,2)
*** Exception: Changing Observation
*Examples.Ex> put goodL (0,2) (0,5)
(0,5)
-}

data Exp = ENum Integer
         | EInc Exp 
         | EFun String Exp 
         | EApp Exp Exp 
         | EVar String
           deriving (Eq, Show)

data Val a = VNum a 
           | VFun String Exp (Env a)
             deriving (Eq, Functor, Foldable, Traversable, Show) 

data Env a = Env [(String, Val a)] deriving (Eq, Functor, Foldable, Traversable, Show) 

lkup x (Env env) = case lookup x env of
                    Just v -> v
                    Nothing -> error $ "Undefined variable: " ++ x
xtnd (x,e) (Env env) = Env $ (x,e):env

incL = Lens (+1) (\_ v -> v - 1)

eval :: Exp -> Env (L s Integer) -> Val (L s Integer)
eval (ENum n) env = VNum (new n)
eval (EInc e) env =
  let VNum n = eval e env
  in VNum (lift incL n)
eval (EFun x e) env =
  VFun x e env
eval (EApp e1 e2) env =
  let VFun x e env' = eval e1 env
      v2 = eval e2 env
  in eval e (xtnd (x,v2) env')
eval (EVar x) env = lkup x env

infixl 9 @@ -- @@ is left associative
(@@) = EApp

expr = twice @@ twice @@ twice @@ twice @@ inc @@ x 
    where
      twice = EFun "f" $ EFun "x" $
                EVar "f"@@ (EVar "f" @@ EVar "x")
      inc   = EFun "x" (EInc (EVar "x"))
      x     = EVar "x"

evalL e = unliftT (\env -> sequenceL $ eval e env)

env0 = Env [("x", VNum 3)]

{-
*Examples.Ex> get (evalL expr) env0
VNum 65539
*Examples.Ex> put (evalL expr) env0 (VNum 65536)
Env [("x",VNum 0)]
-}




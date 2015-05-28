{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Examples.Evaluator where

import Control.LensFunction

import Data.Traversable (Traversable)
import Data.Foldable    (Foldable) 

data Exp = ENum Integer
         | EInc Exp 
         | EFun String Exp 
         | EApp Exp Exp 
         | EVar String
           deriving (Eq, Show)

data Val a = VNum a 
           | VFun String Exp (Env a)
             deriving (Eq, Functor, Foldable, Traversable, Show) 

newtype Env a = Env [(String, Val a)]
              deriving (Eq, Functor, Foldable, Traversable, Show) 

lkup x (Env env) = case lookup x env of
                    Just v -> v
                    Nothing -> error $ "Undefined variable: " ++ x
xtnd (x,e) (Env env) = Env $ (x,e):env

incL = lens' $ \s -> (s + 1, \v -> v - 1)

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

env0   = Env [("x", VNum 3)]
envn n = Env $ [("x", VNum 3)] ++ [  ("y" ++ show i, VNum i)  | i <- [1..n] ]

{-
*Examples.Ex> get (evalL expr) env0
VNum 65539
*Examples.Ex> put (evalL expr) env0 (VNum 65536)
Env [("x",VNum 0)]
-}

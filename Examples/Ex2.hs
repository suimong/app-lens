{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

{- |
This file contains some additional examples.
-}

module Examples.Ex2 where

import Control.LensFunction
import Data.List (elemIndices, splitAt)

import Data.Traversable (Traversable)
import Data.Foldable    (Foldable) 

import qualified Data.Map as M 
import Data.Maybe
import Data.List (nub, findIndex) 

import Control.Applicative ((<$>))
import Control.Category ((.))
import Prelude hiding ((.))

import Control.Lens

filterM p []     = return []
filterM p (x:xs) =
  do b <- liftO p x
     if b then
       (x :) <$> filterM p xs
     else
       filterM p xs

filterL p = unliftMT (\xs -> sequenceL <$> filterM p xs)

{-
*Examples.Ex2> [1,2,3,4] ^. filterL (>2)
[3,4]
*Examples.Ex2> [1,2,3,4] & filterL (>2) .~ [4,5]
[1,2,4,5]
*Examples.Ex2> [1,2,3,4] & filterL (>2) .~ [0,3]
*** Exception: Control.LensFunction.unliftMT: Changed Observation
*Examples.Ex2> [1,2,3,4] & filterL (>2) .~ [1,2,3]
*** Exception: Control.LensFunction.sequenceL: Shape Mismatch
*Examples.Ex2> [1,2,3,4] & filterL (>2) .~ [1]
*** Exception: Control.LensFunction.sequenceL: Shape Mismatch
-}

reverseL = unliftT (\xs -> sequenceL $ reverse xs) 

{-
*Examples.Ex2> [1,2,3] ^. reverseL
[3,2,1]
*Examples.Ex2> [1,2,3] & reverseL .~ [4,5,6]
[6,5,4]
*Examples.Ex2> [1,2,3] & reverseL .~ [4,5]
*** Exception: Control.LensFunction.sequenceL: Shape Mismatch
*Examples.Ex2> [1,2,3] & reverseL .~ [4,5,6,7]
*** Exception: Control.LensFunction.sequenceL: Shape Mismatch
-}


{- Just to show the connection, we implement Voigtlander's bff -}
bff :: (Traversable t1, Traversable t2, Eq a, Eq (t1 ()), Eq (t2 ())) =>
       (forall s. t1 (L s a) -> t2 (L s b)) -> Lens' (t1 a) (t2 b)
bff f = unliftT (\t -> sequenceL $ f t)

{- The following is the bff in our previous work in PPDP 13 -}
bffM :: (Traversable t1, Traversable t2, Eq a, Eq (t1 ()), Eq (t2 ())) =>
        (forall s. t1 (L s a) -> R s (t2 (L s b))) ->
        Lens' (t1 a) (t2 b)
bffM f = unliftMT (\t -> sequenceL <$> f t)



mynub :: Eq b => [L s b] -> R s (L s [b])
mynub = fmap sequenceL . go
    where
      go []     = return []
      go (x:xs) = do xs' <- delete x xs 
                     r   <- go xs'
                     return $ x:r

      delete x []     = return []
      delete x (y:ys) =
          do b <- liftO2 (==) x y 
             r <- delete x ys
             if b then 
                 return r 
             else
                 return $ y:r
                  
data EList a = EList [Int] (M.Map Int a) 
             deriving (Eq, Show, Functor, Foldable, Traversable) 

encode :: Eq a => [a] -> EList a
encode xs = let ys = nub xs 
            in EList [ fromJust $ findIndex (==x) ys | x <- xs]
                     (M.fromList $ zip [0..] ys)

decode :: EList a -> [a]
decode (EList is m) =
    [ fromJust $ M.lookup k m | k <- is ]

mynubL1 :: Eq a => Lens' [a] [a]
mynubL1 = unliftMT mynub

mynubL2 :: Eq a => Lens' [a] [a]
mynubL2 = encodeL . unliftMT (\xs -> mynub (decode xs)) 

encodeL = lens encode (const decode) 

{-
*Examples.Ex2> [1,1,2,3,2] ^. mynubL1
[1,2,3]
*Examples.Ex2> [1,1,2,3,2] & mynubL1 .~ [1,2,6]
[1,1,2,6,2]
*Examples.Ex2> [1,1,2,3,2] & mynubL1 .~ [5,6,7]
*** Exception: Control.LensFunction.unliftMT: Changed Observation

*Examples.Ex2> [1,1,2,3,2] ^. mynubL2
[1,2,3]
*Examples.Ex2> [1,1,2,3,2] & mynubL2 .~ [1,2,6]
[1,1,2,6,2]
*Examples.Ex2> [1,1,2,3,2] & mynubL2 .~ [5,6,7]
[5,5,6,7,6]
-}






{-
MEMO: If we try to write bidirectional version of "lines" in our
framework, we should encounter a limitation of our work.

If we straightforwardly write a lifted monadic "lines", if
from the codes in Prelude, it would have type 

  linesM :: [L s Char] -> L s [[Char]]

because the code contains character-wise construction of string due to
"span".

One would expect that its lifted function should have the type

  linesM :: L s String -> [L s String]

However, this has a problem we cannot change the length of the source
string because "lub" only accepts the strings with the same length.
To make "lub" effectively, we have to know that the input string is
essentially a list of line-separated string. This means that we have
to give a lens corresponding to "lines" manually.

A lesson learned: to use our framework effectively, get must be a
function from a data that have more-explicit structures to ones that
have less-explicit structures.
-}










{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

{- |
This file contains the examples in the paper,
except the lambda-expression evaluator example, which is "Evalulator.hs".
-}

module Examples.Ex where

import Control.LensFunction
import Data.List (elemIndices, splitAt)

import Control.Lens 

-- Explict type declation is needed unless NoMonomorphismRestriction is on. 
tailL = unliftT tailH


tailH xs = list (tail xs)

{-
*Examples.Ex> "abc" ^. tailL
"bc"
*Examples.Ex> "abc" & tailL .~ "Ca"
"aCa"
*Examples.Ex> "abc" & tailL .~ "BC"
"aBC"
*Examples.Ex> "abc" & tailL .~ "BCd"
"*** Exception: Control.LensFunction.list: Shape Mismatch
*Examples.Ex> "abc" & tailL .~ "B"
"*** Exception: Control.LensFunction.list: Shape Mismatch
-}


unlinesH :: [L s String] -> L s String
unlinesH []     = new ""
unlinesH (x:xs) = catLineH x (unlinesH xs)
  where catLineH = lift2 catLineL

catLineL =
  lens (\(x,y) -> (x ++ "\n" ++ y))
       (\ (x,y) v -> let n = length (filter (== '\n') x)
                         i = elemIndices '\n' v !! n 
                         (x',y') = splitAt i v 
                     in (x', tail y'))

unlinesL = unliftT unlinesH 

{-
*Examples.Ex> ["banana", "orange", "apple"] ^. unlinesL
"banana\norange\napple\n"
*Examples.Ex> ["banana", "orange", "apple"] & unlinesL .~ "Banana\nOrange\nApple\n\n"
*** Exception: Control.LensFunction.new: Update on Constant
*Examples.Ex> ["banana", "orange", "apple"] & unlinesL .~ "Banana\nOrange\nApple"
*** Exception: Prelude.(!!): index too large

*Examples.Ex> ["banana", "orange", "apple"] & unlinesL .~ "Banana\nOrange\nApple\n\n"
*** Exception: Control.LensFunction.new: Update on Constant
-}


-- This must be indentity-lens
mustbeID x = lift2 b x unit
  where b = lens (\(x,()) -> x)
                 (\_ x -> (x,()))

{-
*Examples.Ex> "abc" ^. unlift mustbeID
"abc"
*Examples.Ex> "abc" & unlift mustbeID .~ "A"
"A"
*Examples.Ex> "abc" & unlift mustbeID .~ "abc"
"abc"
*Examples.Ex> :t (unlift mustbeID .~)
(unlift mustbeID .~) :: Eq t => t -> t -> t
-}

------------------------------------------------------

mapDefault :: a -> Lens' a b -> Lens' [a] [b]
mapDefault d l = lens (map (view l)) (\s v -> go s v)
  where
    go ss [] = []
    go []     (v:vs) = set l v d : go [] vs 
    go (s:ss) (v:vs) = set l v s : go ss vs



mapH :: Eq a => a -> (forall s. L s a -> L s b) -> L s [a] -> L s [b]
mapH d = liftC (mapDefault d)

mapAddL = unlift (mapH (0,0) addL)

addL = lift (lens (uncurry (+))
            (\(x,_) v -> (x, v - x)))

mapAddL' = unliftT (list . map addL)       

{-
*Examples.Ex> [(1,1), (2,2)] ^. mapAddL
[2,4]
*Examples.Ex> [(1,1), (2,2)] & mapAddL .~ [3,5]
[(1,2),(2,3)]
*Examples.Ex> [(1,1), (2,2)] & mapAddL .~ [3]
[(1,2)]
*Examples.Ex> [(1,1), (2,2)] & mapAddL .~ [3,5,7]
[(1,2),(2,3),(0,7)]

*Examples.Ex> [(1,1), (2,2)] ^. mapAddL'
[2,4]
*Examples.Ex> [(1,1), (2,2)] & mapAddL' .~ [3,5]
[(1,2),(2,3)]
*Examples.Ex> [(1,1), (2,2)] & mapAddL' .~ [3]
*** Exception: Control.LensFunction.list: Shape Mismatch
*Examples.Ex> [(1,1), (2,2)] & mapAddL' .~ [3,5,7]
*** Exception: Control.LensFunction.list: Shape Mismatch
-}

-- toy program for observation 
good x y = fmap (uncurry pair) $ do 
  b <- liftO2 (==) x (new 0)
  return (if b then (x,y) else (x, new 1))

goodL = unliftM2 good   

{-
*Examples.Ex> (0,2) ^. goodL
(0,2)
*Examples.Ex> (0,2) & goodL .~ (1,2)
*** Exception: Control.LensFunction.unliftM2: Changed Observation
*Examples.Ex> (0,2) & goodL .~ (0,6)
(0,6)
-}





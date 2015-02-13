{-# LANGUAGE NoMonomorphismRestriction #-}

module Examples.Ex where

import Data.ApplicativeBX
import Data.List (elemIndices, splitAt)

-- Explict type declation is needed unless NoMonomorphismRestriction is on. 
tailL = unliftT tailH


tailH xs = sequenceL (tail xs)


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



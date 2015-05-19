{- | Very simple implementation of Lens -}

module Data.ApplicativeLens.Lens where

{- |
A variant of conventional representation. 
-}
newtype Lens s v = Lens { runLens :: s -> (v, v -> s) }

get :: Lens s v -> s -> v
get lens = fst . runLens lens
{-# INLINABLE get #-}

put :: Lens s v -> s -> v -> s
put lens = snd . runLens lens
{-# INLINABLE put #-}

modify :: Lens s v -> (v -> v) -> s -> s
modify lens f s =
  let (v, r) = runLens lens s
  in r (f v)
{-# INLINABLE modify #-}     

lens :: (s -> v) -> (s -> v -> s) -> Lens s v
lens g p = Lens (\s -> (g s, \v -> p s v))
{-# INLINABLE lens #-}

lens' :: (s -> (v, v -> s)) -> Lens s v
lens' h = Lens h
{-# INLINABLE lens' #-}

viewrefl = runLens 

(<<<) :: Lens b c -> Lens a b -> Lens a c 
y <<< x = Lens $ \s ->
                  let (v1,r1) = runLens x s
                      (v2,r2) = runLens y v1
                  in (v2, r1 . r2)

(***) :: Lens a s -> Lens b t -> Lens (a,b) (s,t)
x *** y = Lens $ \(a,b) ->
                  let (va,ra) = runLens x a
                      (vb,rb) = runLens y b
                  in ((va,vb), \(va',vb') -> (ra va', rb vb'))



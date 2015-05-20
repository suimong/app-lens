{- | Very simple implementation of Lens -}

module Data.ApplicativeLens.Lens where

data StrictPair a b = StrictPair !a !b 

fstS (StrictPair a _) = a
sndS (StrictPair _ b) = b

{- |
A variant of conventional representation. 
-}
newtype Lens s v = Lens { runLens :: s -> StrictPair v (v -> s) }

get :: Lens s v -> s -> v
get lens = fstS . runLens lens
{-# INLINABLE get #-}

put :: Lens s v -> s -> v -> s
put lens = sndS . runLens lens
{-# INLINABLE put #-}

modify :: Lens s v -> (v -> v) -> s -> s
modify lens f s =
  let StrictPair v r = runLens lens s
  in r (f v)
{-# INLINABLE modify #-}     

lens :: (s -> v) -> (s -> v -> s) -> Lens s v
lens g p = Lens (\s -> StrictPair (g s) (\v -> p s v))
{-# INLINABLE lens #-}

lens' :: (s -> (v, v -> s)) -> Lens s v
lens' h = Lens $ uncurry StrictPair . h
{-# INLINABLE lens' #-}

viewrefl l = \s -> let StrictPair a b = runLens l s in (a,b)
{-# INLINABLE viewrefl #-}

(<<<) :: Lens b c -> Lens a b -> Lens a c 
y <<< x = Lens $ \s ->
                  let StrictPair v1 r1 = runLens x s
                      StrictPair v2 r2 = runLens y v1
                  in StrictPair v2 (r1 . r2)
{-# INLINABLE (<<<) #-}

(***) :: Lens a s -> Lens b t -> Lens (a,b) (s,t)
x *** y = Lens $ \(a,b) ->
                  let StrictPair va ra = runLens x a
                      StrictPair vb rb = runLens y b
                  in StrictPair (va,vb) (\(va',vb') -> (ra va', rb vb'))

{-# INLINABLE (***) #-}


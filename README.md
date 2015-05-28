A Library to Support Applicative Bidirectional Programming with Lenses
======================================================================


This library provides an interface for applicative bidirectional programming.
For example, one can write the following program with our library.


    import Control.LensFunction
    
    unlinesL :: Lens [String] String
    unlinesL = unliftT unlinesH 
    
    unlinesH :: [L s String] -> L s String
    unlinesH []     = new ""
    unlinesH (x:xs) = lift2 catLineL x (unlinesH xs)
    
    catLineL :: Lens (String, String) String
    catLineL = ... {- some appropriate definition -} ...
    

As you can see, we can use the binary lens `catLineL` as a binary
function via a lifting function `lift2`, and construct an involved
bidirectional transformation `unlinesH` in the usual "applicative
programming" style. Then, we can obtain an composed lens as `unlinesL`
via a unlifting function `unliftT`.




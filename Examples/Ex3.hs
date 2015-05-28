module Examples.Ex3 where

import Control.LensFunction
import Data.Map (Map)
import qualified Data.Map as Map 

data Person =
  P { name :: String  
    , address :: Address 
    , salary :: Int }
  deriving (Eq, Show)

data Address =
  A { road :: String
    , city :: String
    , postcode :: String }
  deriving (Eq, Show)


nameH :: L s Person -> L s String
nameH = lift $ lens' $ \s -> (name s, \v -> s { name = v } )

addressH :: L s Person -> L s Address
addressH = lift $ lens' $ \s -> (address s, \v -> s { address = v } )
                    
                        
postcodeH :: L s Address -> L s String
postcodeH = lift $ lens' $ \s -> (postcode s, \v -> s { postcode = v })

testPerson = P { name = "Kazutaka Matsuda"
               , address = A { road = "---"
                             , city = "Sendai"
                             , postcode = "XXX-YYYY" }
               , salary = -1 }

{-
*Examples.Ex3> get (unlift nameH) testPerson
"Kazutaka Matsuda"
*Examples.Ex3> put (unlift nameH) testPerson "Kztk"
P {name = "Kztk", address = A {road = "---", city = "Sendai", postcode = "XXX-YYYY"}, salary = -1}

*Examples.Ex3> get (unlift $ postcodeH . addressH) testPerson
"XXX-YYYY"
*Examples.Ex3> put (unlift $ postcodeH . addressH) testPerson "XXX-ZZZZ"
P {name = "Kazutaka Matsuda", address = A {road = "---", city = "Sendai", postcode = "XXX-ZZZZ"}, salary = -1}
-}


at' :: (Ord k, Eq a) => k -> Lens (Map k a) (Maybe a)
at' k = unliftT (sequenceL . Map.lookup k)

-- NB: We are not allowed to change "Just _" to Nothing and vice versa.

-- To write at, we cannot do it better

at k = lens' $ \m ->
                let mv = Map.lookup k m
                in (mv, \v -> case v of
                               Just t  -> Map.insert k t m
                               Nothing ->
                                 case mv of
                                  Nothing -> m
                                  Just _  -> Map.delete k m)

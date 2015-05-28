{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Control.LensFunction.Exception
       (
         SomeLensFunctionException(..),
         NoLUBException(..),
         ChangedObservationException(..),
         ShapeMismatchException(..),
         ConstantUpdateException(..)
       ) where

import Control.Exception
import Data.Typeable (cast, Typeable)

data SomeLensFunctionException
  = forall e. Exception e => SomeLensFunctionException e
  deriving Typeable

instance Show SomeLensFunctionException where
  show (SomeLensFunctionException e) = show e 

instance Exception SomeLensFunctionException 

lfToException :: Exception e => e -> SomeException
lfToException = toException . SomeLensFunctionException

lfFromException :: Exception e => SomeException -> Maybe e
lfFromException x = do
    SomeLensFunctionException a <- fromException x
    cast a

data NoLUBException = NoLUBException deriving (Typeable)

instance Show NoLUBException where
  show NoLUBException = "No LUB"

instance Exception NoLUBException where
  toException   = lfToException
  fromException = lfFromException

data ChangedObservationException
  = ChangedObservationException
  deriving Typeable

instance Show ChangedObservationException where
  show ChangedObservationException = "Changed Observation"

instance Exception ChangedObservationException where
  toException   = lfToException
  fromException = lfFromException


data ShapeMismatchException = ShapeMismatchException
                               deriving Typeable 

instance Show ShapeMismatchException where
  show ShapeMismatchException = "Shape Mismatch"

instance Exception ShapeMismatchException where
  toException   = lfToException
  fromException = lfFromException

data ConstantUpdateException = ConstantUpdateException
                               deriving Typeable 
instance Show ConstantUpdateException where 
  show ConstantUpdateException = "Update on Constant"

instance Exception ConstantUpdateException where
  toException   = lfToException
  fromException = lfFromException
  





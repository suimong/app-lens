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

data NoLUBException = NoLUBException String deriving (Typeable)

instance Show NoLUBException where
  show (NoLUBException s) = s ++ ": No LUB"

instance Exception NoLUBException where
  toException   = lfToException
  fromException = lfFromException

data ChangedObservationException
  = ChangedObservationException String
  deriving Typeable

instance Show ChangedObservationException where
  show (ChangedObservationException s) = s ++ ": Changed Observation"

instance Exception ChangedObservationException where
  toException   = lfToException
  fromException = lfFromException


data ShapeMismatchException = ShapeMismatchException String
                               deriving Typeable 

instance Show ShapeMismatchException where
  show (ShapeMismatchException s) = s ++ ": Shape Mismatch"

instance Exception ShapeMismatchException where
  toException   = lfToException
  fromException = lfFromException

data ConstantUpdateException = ConstantUpdateException String
                               deriving Typeable 
instance Show ConstantUpdateException where 
  show (ConstantUpdateException s) = s ++ ": Update on Constant"

instance Exception ConstantUpdateException where
  toException   = lfToException
  fromException = lfFromException
  





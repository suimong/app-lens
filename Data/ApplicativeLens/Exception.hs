{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.ApplicativeLens.Exception
       (
         SomeApplicativeLensException(..),
         NoLUBException(..),
         ChangedObservationException(..),
         ShapeMismatchException(..),
         ConstantUpdateException(..)
       ) where

import Control.Exception
import Data.Typeable (cast, Typeable)

data SomeApplicativeLensException
  = forall e. Exception e => SomeApplicativeLensException e
  deriving Typeable

instance Show SomeApplicativeLensException where
  show (SomeApplicativeLensException e) = show e 

instance Exception SomeApplicativeLensException 

alensToException :: Exception e => e -> SomeException
alensToException = toException . SomeApplicativeLensException

alensFromException :: Exception e => SomeException -> Maybe e
alensFromException x = do
    SomeApplicativeLensException a <- fromException x
    cast a

data NoLUBException = NoLUBException deriving (Typeable)

instance Show NoLUBException where
  show NoLUBException = "No LUB"

instance Exception NoLUBException where
  toException   = alensToException
  fromException = alensFromException

data ChangedObservationException
  = ChangedObservationException
  deriving Typeable

instance Show ChangedObservationException where
  show ChangedObservationException = "Changed Observation"

instance Exception ChangedObservationException where
  toException   = alensToException
  fromException = alensFromException


data ShapeMismatchException = ShapeMismatchException
                               deriving Typeable 

instance Show ShapeMismatchException where
  show ShapeMismatchException = "Shape Mismatch"

instance Exception ShapeMismatchException where
  toException   = alensToException
  fromException = alensFromException

data ConstantUpdateException = ConstantUpdateException
                               deriving Typeable 
instance Show ConstantUpdateException where 
  show ConstantUpdateException = "Update on Constant"

instance Exception ConstantUpdateException where
  toException   = alensToException
  fromException = alensFromException
  





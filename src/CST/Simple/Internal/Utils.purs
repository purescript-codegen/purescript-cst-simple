module CST.Simple.Internal.Utils
       ( noteM
       , exceptM
       , requireNonEmptyArray
       ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), note)
import Data.Maybe (Maybe)

noteM :: forall e m a. MonadThrow e m => e -> Maybe a -> m a
noteM e = exceptM <<< note e

exceptM :: forall e m a. MonadThrow e m => Either e a -> m a
exceptM (Left e) = throwError e
exceptM (Right a) = pure a

requireNonEmptyArray :: forall e m a. MonadThrow e m => e -> Array a -> m (NonEmptyArray a)
requireNonEmptyArray e = noteM e <<< NonEmptyArray.fromArray

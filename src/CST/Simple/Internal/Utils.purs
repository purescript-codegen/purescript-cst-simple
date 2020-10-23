module CST.Simple.Internal.Utils
       ( noteM
       ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Maybe (Maybe(..))

noteM :: forall e m a. MonadThrow e m => e -> Maybe a -> m a
noteM e Nothing = throwError e
noteM e (Just a) = pure a

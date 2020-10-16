module Control.Monad.Resource where

import Prelude
import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Control.Monad.Resource.Class (class MonadResource, liftResourceT)
import Control.Monad.Resource.Trans (ResourceT(..))
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref

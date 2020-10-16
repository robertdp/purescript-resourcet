module Control.Monad.Resource where

import Prelude
import Control.Monad.Resource.Class (class MonadResource, liftResourceT)
import Control.Monad.Resource.Trans (ResourceKey(..), ResourceT(..))
import Data.Foldable (for_, traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref as Ref

acquire :: forall a m. MonadResource m => Effect a -> (a -> Effect Unit) -> m (Tuple ResourceKey a)
acquire runAcquire runRelease =
  liftResourceT
    $ ResourceT \poolRef -> do
        state <- Ref.read poolRef
        case state of
          Nothing -> throw "Attempting to acquire from closed pool"
          Just { fresh } -> do
            resource <- runAcquire
            Ref.modify_ (map \s -> s { fresh = add one s.fresh, pool = Map.insert fresh (runRelease resource) s.pool }) poolRef
            pure (Tuple (ResourceKey fresh) resource)

isAcquired :: forall m. MonadResource m => ResourceKey -> m Boolean
isAcquired (ResourceKey key) =
  liftResourceT
    $ ResourceT \poolRef -> do
        state <- Ref.read poolRef
        case state of
          Nothing -> pure false
          Just { pool } -> pure $ Map.member key pool

isReleased :: forall m. MonadResource m => ResourceKey -> m Boolean
isReleased = map not <<< isAcquired

release :: forall m. MonadResource m => ResourceKey -> m Unit
release (ResourceKey key) =
  liftResourceT
    $ ResourceT \poolRef ->
        Ref.read poolRef
          >>= traverse_ \{ pool } ->
              for_ (Map.lookup key pool) \runRelease -> do
                Ref.modify_ (map \s -> s { pool = Map.delete key s.pool }) poolRef
                runRelease

release' :: forall m. MonadResource m => ResourceKey -> m Boolean
release' key = isAcquired key <* release key

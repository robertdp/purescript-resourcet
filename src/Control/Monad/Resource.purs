module Control.Monad.Resource
  ( ResourceKey
  , acquire
  , release
  , release'
  , isAcquired
  , isReleased
  , module Class
  , module Trans
  ) where

import Prelude
import Control.Monad.Resource.Trans (ResourceT(..))
import Control.Monad.Resource.Class (class MonadResource, liftResourceT) as Class
import Control.Monad.Resource.Trans (ResourceT, mapResourceT, runResourceT) as Trans
import Data.Foldable (for_, traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref

newtype ResourceKey
  = ResourceKey Int

derive newtype instance eqResourceKey :: Eq ResourceKey

derive newtype instance ordResourceKey :: Ord ResourceKey

acquire :: forall a m. MonadEffect m => Effect a -> (a -> Effect Unit) -> ResourceT m (Tuple ResourceKey a)
acquire runAcquire runRelease =
  ResourceT \poolRef ->
    liftEffect do
      Ref.read poolRef
        >>= case _ of
            Nothing -> throw "Attempting to acquire from closed pool"
            Just { fresh } -> do
              resource <- runAcquire
              Ref.modify_ (map \s -> s { fresh = add one s.fresh, pool = Map.insert fresh (runRelease resource) s.pool }) poolRef
              pure (Tuple (ResourceKey fresh) resource)

isAcquired :: forall m. MonadEffect m => ResourceKey -> ResourceT m Boolean
isAcquired (ResourceKey key) =
  ResourceT \poolRef ->
    liftEffect do
      Ref.read poolRef
        >>= case _ of
            Nothing -> pure false
            Just { pool } -> pure $ Map.member key pool

isReleased :: forall m. MonadEffect m => ResourceKey -> ResourceT m Boolean
isReleased = map not <<< isAcquired

release :: forall m. MonadEffect m => ResourceKey -> ResourceT m Unit
release (ResourceKey key) =
  ResourceT \poolRef ->
    liftEffect do
      Ref.read poolRef
        >>= traverse_ \{ pool } ->
            for_ (Map.lookup key pool) \runRelease -> do
              Ref.modify_ (map \s -> s { pool = Map.delete key s.pool }) poolRef
              runRelease

release' :: forall m. MonadEffect m => ResourceKey -> ResourceT m Boolean
release' key = isAcquired key <* release key

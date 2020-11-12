module Control.Monad.Resource
  ( register
  , acquire
  , deregister
  , release
  , release'
  , isRegistered
  , isReleased
  , fork
  , forkAff
  , supervise
  , module Exports
  ) where

import Prelude
import Control.Monad.Resource.Class (class MonadResource, liftResourceT)
import Control.Monad.Resource.Class (class MonadResource, liftResourceT) as Exports
import Control.Monad.Resource.Internal.Registry (ReleaseKey)
import Control.Monad.Resource.Internal.Registry (ReleaseKey) as Exports
import Control.Monad.Resource.Internal.Registry as Registry
import Control.Monad.Resource.Trans (Resource, ResourceT(..), mapResourceT, runResource)
import Control.Monad.Resource.Trans (Resource, ResourceT, mapResourceT, runResource, runResourceT) as Exports
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Fiber)
import Effect.Aff as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)

-- | Register some release action to run in the cleanup phase, returning the `ReleaseKey` for the action.
register :: forall m. MonadResource m => Aff Unit -> m ReleaseKey
register runRelease = liftResourceT $ ResourceT \registry -> liftEffect $ Registry.register runRelease registry

-- | Given logic to acquire and free a resource `a`, this will: acquire the resource, register the release action, and
-- | return the resource and the action's `ReleaseKey`
acquire :: forall m a. MonadResource m => Aff a -> (a -> Aff Unit) -> m (Tuple ReleaseKey a)
acquire runAcquire runRelease = do
  resource <- liftAff runAcquire
  key <- register (runRelease resource)
  pure (Tuple key resource)

-- | Remove the release action associated with the key.
deregister :: forall m. MonadResource m => ReleaseKey -> m Unit
deregister key = liftResourceT $ ResourceT \registry -> liftEffect $ Registry.deregister key registry

-- | Trigger the release action for the `ReleaseKey` if it is registered.
release :: forall m. MonadResource m => ReleaseKey -> m Unit
release = liftResourceT <<< ResourceT <<< Registry.release

-- | Trigger the release action for the `ReleaseKey` if it is registered. Returns `true` if the key was found,
-- | `false` otherwise.
release' :: forall m. MonadResource m => ReleaseKey -> m Boolean
release' key = isRegistered key <* release key

-- | Returns `true` if the given `ReleaseKey` is registered in the current `ResourceT`, and `false` otherwise.
isRegistered :: forall m. MonadResource m => ReleaseKey -> m Boolean
isRegistered key = liftResourceT $ ResourceT \registry -> liftEffect $ Registry.has key registry

-- | Returns `true` if the given `ReleaseKey` is not registered in the current `ResourceT`, and `true` otherwise.
isReleased :: forall m. MonadResource m => ReleaseKey -> m Boolean
isReleased = map not <<< isRegistered

fork :: forall a m. MonadResource m => Resource a -> m (Fiber a)
fork child = liftResourceT $ ResourceT \registry -> Registry.forkAff (runResource child) registry

forkAff :: forall a m. MonadResource m => Aff a -> m (Fiber a)
forkAff aff = liftResourceT $ ResourceT \registry -> Registry.forkAff aff registry

supervise :: forall a. Resource a -> Resource a
supervise = mapResourceT Aff.supervise

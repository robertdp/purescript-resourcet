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
  , module Exports
  ) where

import Prelude
import Control.Monad.Resource.Class (class MonadResource, liftResourceT)
import Control.Monad.Resource.Class (class MonadResource, liftResourceT) as Exports
import Control.Monad.Resource.Internal.Registry (ReleaseKey)
import Control.Monad.Resource.Internal.Registry (ReleaseKey) as Exports
import Control.Monad.Resource.Internal.Registry as Registry
import Control.Monad.Resource.Trans (Resource, ResourceT(..), mapResourceT)
import Control.Monad.Resource.Trans (Resource, ResourceT, mapResourceT, runResource, runResourceT) as Exports
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Fiber)
import Effect.Aff as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)

register :: forall m. MonadResource m => Aff Unit -> m ReleaseKey
register runRelease = liftResourceT $ ResourceT \registry -> liftEffect $ Registry.register runRelease registry

acquire :: forall m a. MonadResource m => Aff a -> (a -> Aff Unit) -> m (Tuple ReleaseKey a)
acquire runAcquire runRelease = do
  resource <- liftAff runAcquire
  key <- register (runRelease resource)
  pure (Tuple key resource)

deregister :: forall m. MonadResource m => ReleaseKey -> m Unit
deregister key = liftResourceT $ ResourceT \registry -> liftEffect $ Registry.deregister key registry

release :: forall m. MonadResource m => ReleaseKey -> m Unit
release = liftResourceT <<< ResourceT <<< Registry.release

release' :: forall m. MonadResource m => ReleaseKey -> m Boolean
release' key = isRegistered key <* release key

isRegistered :: forall m. MonadResource m => ReleaseKey -> m Boolean
isRegistered key = liftResourceT $ ResourceT \registry -> liftEffect $ Registry.has key registry

isReleased :: forall m. MonadResource m => ReleaseKey -> m Boolean
isReleased = map not <<< isRegistered

fork :: forall a m. MonadResource m => Resource a -> m (Fiber a)
fork (ResourceT child) = liftResourceT $ ResourceT \registry -> Registry.forkAff (child registry) registry

forkAff :: forall a m. MonadResource m => Aff a -> m (Fiber a)
forkAff aff = liftResourceT $ ResourceT \registry -> Registry.forkAff aff registry

supervise :: forall a. Resource a -> Resource a
supervise = mapResourceT Aff.supervise

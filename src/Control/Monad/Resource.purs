module Control.Monad.Resource
  ( register
  , acquire
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
import Control.Monad.Resource.Map (ReleaseKey)
import Control.Monad.Resource.Map (ReleaseKey) as Exports
import Control.Monad.Resource.Map as Map
import Control.Monad.Resource.Trans (ResourceT(..), runResourceT)
import Control.Monad.Resource.Trans (ResourceT, mapResourceT, runResourceT) as Exports
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Fiber)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)

register :: forall m. MonadResource m => Aff Unit -> m ReleaseKey
register runRelease = liftResourceT $ ResourceT \pool -> liftEffect $ Map.register runRelease pool

acquire :: forall m a. MonadResource m => Aff a -> (a -> Aff Unit) -> m (Tuple ReleaseKey a)
acquire runAcquire runRelease = do
  resource <- liftAff runAcquire
  key <- register (runRelease resource)
  pure (Tuple key resource)

release :: forall m. MonadResource m => ReleaseKey -> m Unit
release = liftResourceT <<< ResourceT <<< Map.release

release' :: forall m. MonadResource m => ReleaseKey -> m Boolean
release' key = isRegistered key <* release key

isRegistered :: forall m. MonadResource m => ReleaseKey -> m Boolean
isRegistered key = liftResourceT $ ResourceT \pool -> liftEffect $ Map.has key pool

isReleased :: forall m. MonadResource m => ReleaseKey -> m Boolean
isReleased = map not <<< isRegistered

fork :: forall a m. MonadResource m => ResourceT Aff a -> m (Fiber a)
fork child = liftResourceT $ ResourceT \pool -> liftEffect $ Map.forkAff (runResourceT child) pool

forkAff :: forall a m. MonadResource m => Aff a -> m (Fiber a)
forkAff aff = liftResourceT $ ResourceT \pool -> liftEffect $ Map.forkAff aff pool

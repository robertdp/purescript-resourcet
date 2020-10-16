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
import Control.Monad.Resource.Pool (ReleaseKey)
import Control.Monad.Resource.Pool (ReleaseKey) as Exports
import Control.Monad.Resource.Pool as Pool
import Control.Monad.Resource.Trans (ResourceT(..))
import Control.Monad.Resource.Trans (ResourceT, mapResourceT, runResourceT) as Exports
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber)
import Effect.Class (liftEffect)

register :: forall m. MonadResource m => Effect Unit -> m ReleaseKey
register = liftResourceT <<< ResourceT <<< Pool.register

acquire :: forall m a. MonadResource m => Effect a -> (a -> Effect Unit) -> m (Tuple ReleaseKey a)
acquire runAcquire runRelease = do
  resource <- liftEffect runAcquire
  key <- register (runRelease resource)
  pure (Tuple key resource)

release :: forall m. MonadResource m => ReleaseKey -> m Unit
release = liftResourceT <<< ResourceT <<< Pool.release

release' :: forall m. MonadResource m => ReleaseKey -> m Boolean
release' key = isRegistered key <* release key

isRegistered :: forall m. MonadResource m => ReleaseKey -> m Boolean
isRegistered = liftResourceT <<< ResourceT <<< Pool.has

isReleased :: forall m. MonadResource m => ReleaseKey -> m Boolean
isReleased = map not <<< isRegistered

fork :: forall a m. MonadResource m => ResourceT Aff a -> m (Tuple ReleaseKey (Fiber a))
fork (ResourceT run) = liftResourceT $ ResourceT \pool -> Pool.forkAff (run pool) pool

forkAff :: forall a m. MonadResource m => Aff a -> m (Tuple ReleaseKey (Fiber a))
forkAff aff = liftResourceT $ ResourceT \pool -> Pool.forkAff aff pool

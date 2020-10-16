module Control.Monad.Resource
  ( register
  , acquire
  , release
  , release'
  , isRegistered
  , isReleased
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
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref

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

fork :: forall a. ResourceT Aff a -> ResourceT Effect (Tuple ReleaseKey (Fiber a))
fork (ResourceT run) = do
  canceler <- liftEffect $ Ref.new Nothing
  key <- register (Ref.read canceler >>= sequence_)
  fiber <-
    liftResourceT
      $ ResourceT \pool ->
          Aff.launchAff
            $ Aff.cancelWith (run pool)
            $ Aff.effectCanceler (Pool.release key pool)
  liftEffect $ Ref.write (Just (Aff.launchAff_ (Aff.killFiber (Aff.error "Killed by resource cleanup") fiber))) canceler
  pure (Tuple key fiber)

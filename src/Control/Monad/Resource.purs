module Control.Monad.Resource
  ( ReleaseKey
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

newtype ReleaseKey
  = ReleaseKey Int

derive newtype instance eqReleaseKey :: Eq ReleaseKey

derive newtype instance ordReleaseKey :: Ord ReleaseKey

register :: forall m. MonadEffect m => Effect Unit -> ResourceT m ReleaseKey
register runRelease =
  ResourceT \poolRef ->
    liftEffect do
      Ref.read poolRef
        >>= case _ of
            Nothing -> throw "Attempting to acquire from closed pool"
            Just { fresh: key } -> do
              Ref.modify_
                ( map \state ->
                    { fresh: add one state.fresh
                    , pool: Map.insert key runRelease state.pool
                    }
                )
                poolRef
              pure (ReleaseKey key)

acquire :: forall a m. MonadEffect m => Effect a -> (a -> Effect Unit) -> ResourceT m (Tuple ReleaseKey a)
acquire runAcquire runRelease = do
  resource <- liftEffect runAcquire
  key <- register (runRelease resource)
  pure (Tuple key resource)

release :: forall m. MonadEffect m => ReleaseKey -> ResourceT m Unit
release (ReleaseKey key) =
  ResourceT \poolRef ->
    liftEffect do
      Ref.read poolRef
        >>= traverse_ \{ pool } ->
            for_ (Map.lookup key pool) \runRelease -> do
              Ref.modify_ (map \state -> state { pool = Map.delete key state.pool }) poolRef
              runRelease

release' :: forall m. MonadEffect m => ReleaseKey -> ResourceT m Boolean
release' key = isAcquired key <* release key

isAcquired :: forall m. MonadEffect m => ReleaseKey -> ResourceT m Boolean
isAcquired (ReleaseKey key) =
  ResourceT \poolRef ->
    liftEffect do
      Ref.read poolRef
        >>= case _ of
            Nothing -> pure false
            Just { pool } -> pure $ Map.member key pool

isReleased :: forall m. MonadEffect m => ReleaseKey -> ResourceT m Boolean
isReleased = map not <<< isAcquired

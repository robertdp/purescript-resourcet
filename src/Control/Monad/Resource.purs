module Control.Monad.Resource
  ( ReleaseKey
  , register
  , acquire
  , release
  , release'
  , isRegistered
  , isReleased
  , module Class
  , module Trans
  ) where

import Prelude
import Control.Monad.Resource.Class (class MonadResource, liftResourceT)
import Control.Monad.Resource.Class (class MonadResource, liftResourceT) as Class
import Control.Monad.Resource.Trans (ResourceT(..))
import Control.Monad.Resource.Trans (ResourceT, mapResourceT, runResourceT) as Trans
import Data.Foldable (for_, traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref

newtype ReleaseKey
  = ReleaseKey Int

derive newtype instance eqReleaseKey :: Eq ReleaseKey

derive newtype instance ordReleaseKey :: Ord ReleaseKey

register :: forall m. MonadResource m => Effect Unit -> m ReleaseKey
register runRelease =
  liftResourceT
    $ ResourceT \poolRef ->
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

acquire :: forall m a. MonadResource m => Effect a -> (a -> Effect Unit) -> m (Tuple ReleaseKey a)
acquire runAcquire runRelease = do
  resource <- liftEffect runAcquire
  key <- register (runRelease resource)
  pure (Tuple key resource)

release :: forall m. MonadResource m => ReleaseKey -> m Unit
release (ReleaseKey key) =
  liftResourceT
    $ ResourceT \poolRef ->
        liftEffect do
          Ref.read poolRef
            >>= traverse_ \{ pool } ->
                for_ (Map.lookup key pool) \runRelease -> do
                  Ref.modify_ (map \state -> state { pool = Map.delete key state.pool }) poolRef
                  runRelease

release' :: forall m. MonadResource m => ReleaseKey -> m Boolean
release' key = isRegistered key <* release key

isRegistered :: forall m. MonadResource m => ReleaseKey -> m Boolean
isRegistered (ReleaseKey key) =
  liftResourceT
    $ ResourceT \poolRef ->
        liftEffect do
          Ref.read poolRef
            >>= case _ of
                Nothing -> pure false
                Just { pool } -> pure $ Map.member key pool

isReleased :: forall m. MonadResource m => ReleaseKey -> m Boolean
isReleased = map not <<< isRegistered

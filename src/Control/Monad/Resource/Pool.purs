module Control.Monad.Resource.Pool where

import Prelude
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Foldable (for_, traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref

newtype ResourcePool
  = ResourcePool
  ( Ref
      ( Maybe
          { fresh :: Int
          , map :: Map Int (Effect Unit)
          }
      )
  )

newtype ReleaseKey
  = ReleaseKey Int

derive newtype instance eqReleaseKey :: Eq ReleaseKey

derive newtype instance ordReleaseKey :: Ord ReleaseKey

register :: Effect Unit -> ResourcePool -> Effect ReleaseKey
register runRelease (ResourcePool ref) =
  Ref.read ref
    >>= case _ of
        Nothing -> throw "Attempting to acquire from closed pool"
        Just { fresh: key } -> do
          Ref.modify_
            ( map \state ->
                { fresh: add one state.fresh
                , map: Map.insert key runRelease state.map
                }
            )
            ref
          pure (ReleaseKey key)

release :: ReleaseKey -> ResourcePool -> Effect Unit
release (ReleaseKey key) (ResourcePool ref) =
  Ref.read ref
    >>= traverse_ \state ->
        for_ (Map.lookup key state.map) \runRelease -> do
          Ref.modify_ (map \s -> s { map = Map.delete key s.map }) ref
          runRelease

has :: ReleaseKey -> ResourcePool -> Effect Boolean
has (ReleaseKey key) (ResourcePool ref) =
  Ref.read ref
    >>= case _ of
        Nothing -> pure false
        Just state -> pure $ Map.member key state.map

finalize :: ResourcePool -> Effect Unit
finalize (ResourcePool ref) = tailRecM go unit
  where
  go _ = do
    Ref.read ref
      >>= case _ of
          Nothing -> pure (Done unit)
          Just state -> case Map.findMax state.map of
            Nothing -> do
              Ref.write Nothing ref
              pure (Done unit)
            Just { key, value } -> do
              Ref.modify_ (map \s -> s { map = Map.delete key s.map }) ref
              value
              pure (Loop unit)

createEmpty :: Effect ResourcePool
createEmpty = ResourcePool <$> Ref.new initialState
  where
  initialState = Just { fresh: 0, map: Map.empty }

module Control.Monad.Resource.Map where

import Prelude
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref

newtype ReleaseMap
  = ReleaseMap
  ( Ref
      ( Maybe
          { nextKey :: Int
          , references :: Int
          , releasers :: Map Int (Aff Unit)
          }
      )
  )

newtype ReleaseKey
  = ReleaseKey Int

derive newtype instance eqReleaseKey :: Eq ReleaseKey

derive newtype instance ordReleaseKey :: Ord ReleaseKey

register :: Aff Unit -> ReleaseMap -> Effect ReleaseKey
register runRelease (ReleaseMap ref) =
  Ref.read ref
    >>= case _ of
        Nothing -> throw "Attempting to acquire from closed pool"
        Just { nextKey } -> do
          Ref.modify_
            ( map \state ->
                state
                  { nextKey = add one state.nextKey
                  , releasers = Map.insert nextKey runRelease state.releasers
                  }
            )
            ref
          pure (ReleaseKey nextKey)

release :: ReleaseKey -> ReleaseMap -> Aff Unit
release (ReleaseKey key) (ReleaseMap ref) =
  join
    $ liftEffect
    $ Ref.read ref
    >>= case _ of
        Nothing -> mempty
        Just { releasers } -> case Map.lookup key releasers of
          Nothing -> mempty
          Just runRelease -> do
            Ref.modify_ (map \s -> s { releasers = Map.delete key s.releasers }) ref
            pure runRelease

has :: ReleaseKey -> ReleaseMap -> Effect Boolean
has (ReleaseKey key) (ReleaseMap ref) =
  Ref.read ref
    >>= case _ of
        Nothing -> pure false
        Just state -> pure $ Map.member key state.releasers

reference :: ReleaseMap -> Effect Unit
reference (ReleaseMap ref) = Ref.modify_ (map \s -> s { references = add one s.references }) ref

finalize :: ReleaseMap -> Aff Unit
finalize pool@(ReleaseMap ref) = do
  state <- liftEffect $ Ref.modify (map \s -> s { references = sub one s.references }) ref
  case state of
    Just { references }
      | references == zero -> releaseAll pool
    _ -> pure unit

releaseAll :: ReleaseMap -> Aff Unit
releaseAll (ReleaseMap ref) = tailRecM go unit
  where
  go _ = do
    mostRecent <- extractMostRecent
    case mostRecent of
      Nothing -> do
        liftEffect $ Ref.write Nothing ref
        pure (Done unit)
      Just runRelease -> do
        runRelease
        pure (Loop unit)

  extractMostRecent =
    liftEffect
      $ Ref.read ref
      >>= case _ of
          Nothing -> pure Nothing
          Just { releasers } -> case Map.findMax releasers of
            Nothing -> pure Nothing
            Just { key, value } -> do
              Ref.modify_ (map \s -> s { releasers = Map.delete key s.releasers }) ref
              pure (Just value)

createEmpty :: Effect ReleaseMap
createEmpty = ReleaseMap <$> Ref.new initialState
  where
  initialState = Just { nextKey: 0, references: 1, releasers: Map.empty }

forkAff :: forall a. Aff a -> ReleaseMap -> Effect (Fiber a)
forkAff aff pool = do
  fiberRef <- Ref.new Nothing
  let
    killFiber =
      liftEffect (Ref.read fiberRef)
        >>= traverse_ (Aff.killFiber (Aff.error "Killed by resource cleanup"))
  key <- register (killFiber *> finalize pool) pool
  fiber <-
    Aff.launchAff
      $ Aff.cancelWith aff
      $ Aff.effectCanceler (Aff.launchAff_ $ release key pool)
  Ref.write (Just fiber) fiberRef
  reference pool
  pure fiber

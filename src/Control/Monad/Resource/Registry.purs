module Control.Monad.Resource.Registry where

import Prelude
import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.Either (either)
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

newtype Registry
  = Registry
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

register :: Aff Unit -> Registry -> Effect ReleaseKey
register runRelease (Registry ref) =
  Ref.read ref
    >>= case _ of
        Nothing -> throw "Attempting to acquire from closed registry"
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

release :: ReleaseKey -> Registry -> Aff Unit
release (ReleaseKey key) (Registry ref) =
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

has :: ReleaseKey -> Registry -> Effect Boolean
has (ReleaseKey key) (Registry ref) =
  Ref.read ref
    >>= case _ of
        Nothing -> pure false
        Just state -> pure $ Map.member key state.releasers

reference :: Registry -> Effect Unit
reference (Registry ref) = Ref.modify_ (map \s -> s { references = add one s.references }) ref

finalize :: Registry -> Aff Unit
finalize registry@(Registry ref) = do
  state <- liftEffect $ Ref.modify (map \s -> s { references = sub one s.references }) ref
  case state of
    Just { references }
      | references == zero -> releaseAll registry
    _ -> pure unit

releaseAll :: Registry -> Aff Unit
releaseAll (Registry ref) = tailRecM go []
  where
  go errors =
    extractMostRecent
      >>= case _ of
          Nothing -> do
            liftEffect $ Ref.write Nothing ref
            traverse_ throwError errors
            pure $ Done unit
          Just runRelease ->
            Loop
              <$> either (Array.snoc errors) (const errors)
              <$> try runRelease

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

createEmpty :: Effect Registry
createEmpty = Registry <$> Ref.new initialState
  where
  initialState = Just { nextKey: 0, references: 1, releasers: Map.empty }

forkAff :: forall a. Aff a -> Registry -> Effect (Fiber a)
forkAff aff registry = do
  reference registry
  fiberRef <- Ref.new Nothing
  let
    killFiber =
      liftEffect (Ref.read fiberRef)
        >>= traverse_ (Aff.killFiber (Aff.error "Killed by resource cleanup"))
  key <- register (killFiber *> finalize registry) registry
  fiber <-
    Aff.launchAff
      $ Aff.cancelWith (aff <* release key registry)
      $ Aff.effectCanceler (Aff.launchAff_ $ release key registry)
  Ref.write (Just fiber) fiberRef
  pure fiber

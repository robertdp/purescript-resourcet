module Control.Monad.Resource.Internal.Registry where

import Prelude
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Either (either)
import Data.Foldable (traverse_)
import Data.List (List(..), reverse, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, Fiber, throwError)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref

newtype Registry = Registry
  ( Ref
      ( Maybe
          { nextKey :: Int
          , references :: Int
          , releasers :: Map Int (Aff Unit)
          }
      )
  )

newtype ReleaseKey = ReleaseKey Int

derive newtype instance Eq ReleaseKey

derive newtype instance Ord ReleaseKey

register :: Aff Unit -> Registry -> Effect ReleaseKey
register runRelease (Registry ref) =
  Ref.read ref
    >>= case _ of
      Just { nextKey } -> do
        Ref.modify_
          (map \state -> state { nextKey = add one state.nextKey, releasers = Map.insert nextKey runRelease state.releasers })
          ref
        pure (ReleaseKey nextKey)
      Nothing -> throw "Attempting to acquire from closed registry"

release :: ReleaseKey -> Registry -> Aff Unit
release (ReleaseKey key) (Registry ref) = join <<< liftEffect $ do
  Ref.read ref
    >>= case _ of
      Just { releasers } -> case Map.lookup key releasers of
        Just runRelease -> do
          Ref.modify_ (map \s -> s { releasers = Map.delete key s.releasers }) ref
          pure runRelease
        Nothing -> mempty
      Nothing -> mempty

deregister :: ReleaseKey -> Registry -> Effect Unit
deregister (ReleaseKey key) (Registry ref) =
  Ref.modify_
    (map \s -> s { releasers = Map.delete key s.releasers })
    ref

has :: ReleaseKey -> Registry -> Effect Boolean
has (ReleaseKey key) (Registry ref) =
  Ref.read ref <#>
    case _ of
      Just state -> Map.member key state.releasers
      Nothing -> false

reference :: Registry -> Aff Unit
reference (Registry ref) = liftEffect $
  Ref.modify_ (map \s -> s { references = s.references + one }) ref

cleanup :: Registry -> Aff Unit
cleanup registry@(Registry ref) =
  Aff.invincible do
    state <- liftEffect $ Ref.modify (map \s -> s { references = s.references - one }) ref
    case state of
      Just { references }
        | references == zero -> releaseAll registry >>= traverse_ throwError
      _ -> pure unit

releaseAll :: Registry -> Aff (List Error)
releaseAll (Registry ref) = reverse <$> tailRecM go Nil
  where
  go errors =
    liftEffect extractMostRecent
      >>= case _ of
        Just runRelease -> do
          result <- Aff.attempt runRelease
          pure $ Loop $ either (_ : errors) (const errors) result
        Nothing -> do
          liftEffect $ Ref.write Nothing ref
          pure $ Done errors

  extractMostRecent =
    Ref.read ref
      >>= case _ of
        Just { releasers } -> case Map.findMax releasers of
          Just { key, value } -> do
            Ref.modify_ (map \s -> s { releasers = Map.delete key s.releasers }) ref
            pure (Just value)
          Nothing -> pure Nothing
        Nothing -> pure Nothing

createEmpty :: Effect Registry
createEmpty = Registry <$> Ref.new initialState
  where
  initialState = Just { nextKey: 0, references: 1, releasers: Map.empty }

forkAff :: forall a. Aff a -> Registry -> Aff (Fiber a)
forkAff aff registry = do
  reference registry
  Aff.forkAff $ Aff.finally (cleanup registry) aff

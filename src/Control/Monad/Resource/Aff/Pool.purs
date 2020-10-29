module Control.Monad.Resource.Aff.Pool where

import Prelude
import Control.Monad.Resource (class MonadResource)
import Control.Monad.Resource as Resource
import Data.Foldable (traverse_)
import Data.Map as Map
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref

create :: forall m a. MonadResource m => m (Aff a -> Aff a)
create = do
  pool <-
    liftEffect ado
      fresh <- Ref.new 0
      fibers <- Ref.new Map.empty
      in { fresh, fibers }
  let
    cleanup = do
      fibers <- liftEffect $ Ref.modify' (\fibers -> { state: Map.empty, value: fibers }) pool.fibers
      traverse_ (Aff.killFiber (Aff.error "Cancelling")) fibers
  _ <- Resource.register cleanup
  pure \aff -> do
    fiber <-
      liftEffect do
        id <- Ref.modify' (\fresh -> { state: fresh + 1, value: fresh }) pool.fresh
        doneRef <- Ref.new false
        fiber <-
          Aff.launchAff
            $ Aff.finally
                ( liftEffect do
                    Ref.modify_ (Map.delete id) pool.fibers
                    Ref.write true doneRef
                )
                aff
        unlessM (Ref.read doneRef) do
          Ref.modify_ (Map.insert id (void fiber)) pool.fibers
        pure fiber
    Aff.joinFiber fiber

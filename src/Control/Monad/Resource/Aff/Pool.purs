module Control.Monad.Resource.Aff.Pool where

import Prelude
import Control.Monad.Resource (class MonadResource)
import Control.Monad.Resource as Resource
import Data.Foldable (traverse_)
import Data.Map as Map
import Effect.Aff (Aff, throwError)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref

-- | Create a function that will track all running Aff fibers and kill them in the resource cleanup.
create :: forall m a. MonadResource m => m (Aff a -> Aff a)
create = do
  pool <-
    liftEffect ado
      closed <- Ref.new false
      fibers <- Ref.new Map.empty
      fresh <- Ref.new 0
      in { closed, fibers, fresh }
  _ <-
    Resource.register do
      liftEffect $ Ref.write true pool.closed
      fibers <- liftEffect $ Ref.modify' (\fibers -> { state: Map.empty, value: fibers }) pool.fibers
      traverse_ (Aff.killFiber (Aff.error "Cancelling")) fibers
  pure \aff -> do
    whenM (liftEffect $ Ref.read pool.closed) do
      throwError (Aff.error "Pool has been closed, cannot track new fibers")
    Aff.joinFiber
      =<< liftEffect do
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

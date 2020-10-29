module Control.Monad.Resource.Aff.Pool where

import Prelude
import Control.Monad.Resource (class MonadResource)
import Control.Monad.Resource as Resource
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map as Map
import Effect.Aff (Aff, Fiber)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

newtype Pool
  = Pool
  { fresh :: Ref Int
  , fibers :: Ref (Map Int (Fiber Unit))
  }

create :: forall m. MonadResource m => m Pool
create = do
  pool <-
    liftEffect ado
      fresh <- Ref.new 0
      fibers <- Ref.new Map.empty
      in { fresh, fibers }
  let
    cleanup = do
      fibers <- liftEffect $ Ref.modify' (\s -> { state: Map.empty, value: s }) pool.fibers
      traverse_ (Aff.killFiber (Aff.error "Cancelling")) fibers
  _ <- Resource.register cleanup
  pure (Pool pool)

run :: forall a. Pool -> Aff a -> Aff a
run (Pool pool) aff = do
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

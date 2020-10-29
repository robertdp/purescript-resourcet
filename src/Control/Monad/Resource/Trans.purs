module Control.Monad.Resource.Trans where

import Prelude
import Control.Monad.Cont (class MonadCont, callCC)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.Resource.Registry (Registry)
import Control.Monad.Resource.Registry as Registry
import Control.Monad.State (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (class MonadTell, class MonadWriter, listen, pass, tell)
import Control.MonadPlus (class Alt, class Alternative, class MonadPlus, class MonadZero, class Plus, alt, empty)
import Control.Parallel (class Parallel, parallel, sequential)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)

newtype ResourceT m a
  = ResourceT (Registry -> m a)

type Resource
  = ResourceT Aff

mapResourceT :: forall m m' a b. (m a -> m' b) -> ResourceT m a -> ResourceT m' b
mapResourceT f (ResourceT r) = ResourceT (f <<< r)

runResourceT :: forall m a. MonadAff m => m ~> Aff -> ResourceT m a -> Aff a
runResourceT nat (ResourceT run) = do
  registry <- liftEffect Registry.createEmpty
  Aff.finally (Registry.cleanup registry) (nat (run registry))

runResource :: forall a. Resource a -> Aff a
runResource = runResourceT identity

flattenResourceT :: forall a m. ResourceT (ResourceT m) a -> ResourceT m a
flattenResourceT (ResourceT run) = ResourceT \registry -> case run registry of ResourceT run' -> run' registry

instance functorResourceT :: Functor m => Functor (ResourceT m) where
  map f (ResourceT r) = ResourceT (map f <<< r)

instance applyResourceT :: Apply m => Apply (ResourceT m) where
  apply (ResourceT f) (ResourceT a) = ResourceT \r -> apply (f r) (a r)

instance applicativeResourceT :: Applicative m => Applicative (ResourceT m) where
  pure a = ResourceT \_ -> pure a

instance bindResourceT :: Bind m => Bind (ResourceT m) where
  bind (ResourceT run) f =
    ResourceT \registry -> do
      a <- run registry
      case f a of
        ResourceT run' -> run' registry

instance monadResourceT :: Monad m => Monad (ResourceT m)

instance monadTransResourceT :: MonadTrans ResourceT where
  lift m = ResourceT \_ -> m

instance monadAskResourceT :: MonadAsk r m => MonadAsk r (ResourceT m) where
  ask = lift ask

instance monadReaderResourceT :: MonadReader r m => MonadReader r (ResourceT m) where
  local = mapResourceT <<< local

instance monadTellResourceT :: MonadTell w m => MonadTell w (ResourceT m) where
  tell = lift <<< tell

instance monadWriterResourceT :: MonadWriter w m => MonadWriter w (ResourceT m) where
  listen = mapResourceT listen
  pass = mapResourceT pass

instance monadStateResourceT :: MonadState s m => MonadState s (ResourceT m) where
  state = lift <<< state

instance monadContResourceT :: MonadCont m => MonadCont (ResourceT m) where
  callCC f = ResourceT \registry -> callCC \k -> case f (lift <<< k) of ResourceT run -> run registry

instance monadThrowResourceT :: MonadThrow e m => MonadThrow e (ResourceT m) where
  throwError = lift <<< throwError

instance monadErrorResourceT :: MonadError e m => MonadError e (ResourceT m) where
  catchError (ResourceT run) h =
    ResourceT \registry ->
      catchError (run registry) \e -> case h e of
        ResourceT run' -> run' registry

instance monadEffectResourceT :: MonadEffect m => MonadEffect (ResourceT m) where
  liftEffect = lift <<< liftEffect

instance monadAffResourceT :: MonadAff m => MonadAff (ResourceT m) where
  liftAff = lift <<< liftAff

instance monadRecResourceT :: MonadRec m => MonadRec (ResourceT m) where
  tailRecM k a = ResourceT \registry -> tailRecM (\a' -> case k a' of ResourceT run -> run registry) a

instance altResourceT :: (Monad m, Alt m) => Alt (ResourceT m) where
  alt (ResourceT run) (ResourceT run') = ResourceT \registry -> alt (run registry) (run' registry)

instance plusResourceT :: (Monad m, Plus m) => Plus (ResourceT m) where
  empty = lift empty

instance alternativeResourceT :: (Monad m, Alternative m) => Alternative (ResourceT m)

instance monadZeroResourceT :: MonadZero m => MonadZero (ResourceT m)

instance monadPlusResourceT :: MonadPlus m => MonadPlus (ResourceT m)

instance parResourceT :: Parallel f m => Parallel (ResourceT f) (ResourceT m) where
  parallel = mapResourceT parallel
  sequential = mapResourceT sequential

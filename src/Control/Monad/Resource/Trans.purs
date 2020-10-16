module Control.Monad.Resource.Trans where

import Prelude
import Control.Monad.Cont (class MonadCont, callCC)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.Resource.Pool (ResourcePool)
import Control.Monad.Resource.Pool as Pool
import Control.Monad.State (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (class MonadTell, class MonadWriter, listen, pass, tell)
import Control.MonadPlus (class Alt, class Alternative, class MonadPlus, class MonadZero, class Plus, alt, empty)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)

newtype ResourceT m a
  = ResourceT (ResourcePool -> m a)

mapResourceT :: forall m m' a b. (m a -> m' b) -> ResourceT m a -> ResourceT m' b
mapResourceT f (ResourceT r) = ResourceT (f <<< r)

runResourceT :: forall m e a. MonadEffect m => MonadError e m => ResourceT m a -> m a
runResourceT (ResourceT runResource) = do
  pool <- liftEffect Pool.createEmpty
  let
    cleanup = liftEffect (Pool.finalize pool)
  catchError (runResource pool <* cleanup) (\e -> cleanup *> throwError e)

instance functorResourceT :: Monad m => Functor (ResourceT m) where
  map f (ResourceT r) = ResourceT (map f <<< r)

instance applyResourceT :: Monad m => Apply (ResourceT m) where
  apply = ap

instance applicativeResourceT :: Monad m => Applicative (ResourceT m) where
  pure a = ResourceT \_ -> pure a

instance bindResourceT :: Monad m => Bind (ResourceT m) where
  bind (ResourceT r) f =
    ResourceT \p -> do
      a <- r p
      case f a of
        ResourceT r' -> r' p

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
  callCC f = ResourceT \p -> callCC \k -> case f (lift <<< k) of ResourceT r -> r p

instance monadThrowResourceT :: MonadThrow e m => MonadThrow e (ResourceT m) where
  throwError = lift <<< throwError

instance monadErrorResourceT :: MonadError e m => MonadError e (ResourceT m) where
  catchError (ResourceT r) h =
    ResourceT \p ->
      catchError (r p) \e -> case h e of
        ResourceT r' -> r' p

instance monadEffectResourceT :: MonadEffect m => MonadEffect (ResourceT m) where
  liftEffect = lift <<< liftEffect

instance monadAffResourceT :: MonadAff m => MonadAff (ResourceT m) where
  liftAff = lift <<< liftAff

instance monadRecResourceT :: MonadRec m => MonadRec (ResourceT m) where
  tailRecM k a = ResourceT \p -> tailRecM (\a' -> case k a' of ResourceT r -> pure =<< r p) a

instance altResourceT :: (Monad m, Alt m) => Alt (ResourceT m) where
  alt (ResourceT r) (ResourceT r') = ResourceT \p -> alt (r p) (r' p)

instance plusResourceT :: (Monad m, Plus m) => Plus (ResourceT m) where
  empty = lift empty

instance alternativeResourceT :: (Monad m, Alternative m) => Alternative (ResourceT m)

instance monadZeroResourceT :: MonadZero m => MonadZero (ResourceT m)

instance monadPlusResourceT :: MonadPlus m => MonadPlus (ResourceT m)

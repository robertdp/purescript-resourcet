module Control.Monad.Resource.Class where

import Prelude
import Control.Monad.Cont (ContT)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.List.Trans (ListT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.RWS.Trans (RWST)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.Resource.Trans (ResourceT, mapResourceT)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)

class
  MonadAff m <= MonadResource m where
  liftResourceT :: forall a. ResourceT Aff a -> m a

instance monadResourceResourceT :: MonadAff m => MonadResource (ResourceT m) where
  liftResourceT = mapResourceT liftAff

instance monadResourceContT :: MonadResource m => MonadResource (ContT r m) where
  liftResourceT = lift <<< liftResourceT

instance monadResourceExceptT :: MonadResource m => MonadResource (ExceptT e m) where
  liftResourceT = lift <<< liftResourceT

instance monadResourceListT :: MonadResource m => MonadResource (ListT m) where
  liftResourceT = lift <<< liftResourceT

instance monadResourceMaybeT :: MonadResource m => MonadResource (MaybeT m) where
  liftResourceT = lift <<< liftResourceT

instance monadResourceReaderT :: MonadResource m => MonadResource (ReaderT r m) where
  liftResourceT = lift <<< liftResourceT

instance monadResourceRWST :: (MonadResource m, Monoid w) => MonadResource (RWST r w s m) where
  liftResourceT = lift <<< liftResourceT

instance monadResourceStateT :: MonadResource m => MonadResource (StateT s m) where
  liftResourceT = lift <<< liftResourceT

instance monadResourceWriterT :: (MonadResource m, Monoid w) => MonadResource (WriterT w m) where
  liftResourceT = lift <<< liftResourceT

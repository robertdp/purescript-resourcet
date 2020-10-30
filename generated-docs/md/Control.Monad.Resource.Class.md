## Module Control.Monad.Resource.Class

#### `MonadResource`

``` purescript
class (MonadAff m) <= MonadResource m  where
  liftResourceT :: forall a. ResourceT Aff a -> m a
```

##### Instances
``` purescript
(MonadAff m) => MonadResource (ResourceT m)
(MonadResource m) => MonadResource (ContT r m)
(MonadResource m) => MonadResource (ExceptT e m)
(MonadResource m) => MonadResource (ListT m)
(MonadResource m) => MonadResource (MaybeT m)
(MonadResource m) => MonadResource (ReaderT r m)
(MonadResource m, Monoid w) => MonadResource (RWST r w s m)
(MonadResource m) => MonadResource (StateT s m)
(MonadResource m, Monoid w) => MonadResource (WriterT w m)
```



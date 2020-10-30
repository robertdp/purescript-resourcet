## Module Control.Monad.Resource.Trans

#### `ResourceT`

``` purescript
newtype ResourceT m a
  = ResourceT (Registry -> m a)
```

The resource cleanup monad transformer.

This monad transformer extends the base monad with a mutable registry for tracking resource cleanup actions.
At the end of evaluation all remaining cleanup actions are executed.

##### Instances
``` purescript
(Functor m) => Functor (ResourceT m)
(Apply m) => Apply (ResourceT m)
(Applicative m) => Applicative (ResourceT m)
(Bind m) => Bind (ResourceT m)
(Monad m) => Monad (ResourceT m)
MonadTrans ResourceT
(MonadAsk r m) => MonadAsk r (ResourceT m)
(MonadReader r m) => MonadReader r (ResourceT m)
(MonadTell w m) => MonadTell w (ResourceT m)
(MonadWriter w m) => MonadWriter w (ResourceT m)
(MonadState s m) => MonadState s (ResourceT m)
(MonadCont m) => MonadCont (ResourceT m)
(MonadThrow e m) => MonadThrow e (ResourceT m)
(MonadError e m) => MonadError e (ResourceT m)
(MonadEffect m) => MonadEffect (ResourceT m)
(MonadAff m) => MonadAff (ResourceT m)
(MonadRec m) => MonadRec (ResourceT m)
(Monad m, Alt m) => Alt (ResourceT m)
(Monad m, Plus m) => Plus (ResourceT m)
(Monad m, Alternative m) => Alternative (ResourceT m)
(MonadZero m) => MonadZero (ResourceT m)
(MonadPlus m) => MonadPlus (ResourceT m)
(Parallel f m) => Parallel (ResourceT f) (ResourceT m)
```

#### `Resource`

``` purescript
type Resource = ResourceT Aff
```

The `Resource` monad is a synonym for `ResourceT Aff`.

#### `mapResourceT`

``` purescript
mapResourceT :: forall m m' a b. (m a -> m' b) -> ResourceT m a -> ResourceT m' b
```

Change the type of the result in a `ResourceT` monad action.

#### `runResourceT`

``` purescript
runResourceT :: forall m a b. (m a -> Aff b) -> ResourceT m a -> Aff b
```

Run a computation in the `ResourceT` monad, while changing it to `Aff` to ensure the cleanup will run safely.

#### `runResource`

``` purescript
runResource :: forall a. Resource a -> Aff a
```

Run an `Aff` computation in the `ResourceT` monad.

#### `flattenResourceT`

``` purescript
flattenResourceT :: forall m a. ResourceT (ResourceT m) a -> ResourceT m a
```

Combine two levels of `ResourceT` into one, so that they also share the same cleanup store.



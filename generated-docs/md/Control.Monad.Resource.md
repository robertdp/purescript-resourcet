## Module Control.Monad.Resource

#### `register`

``` purescript
register :: forall m. MonadResource m => Aff Unit -> m ReleaseKey
```

Register some release action to run in the cleanup phase, returning the `ReleaseKey` for the action.

#### `acquire`

``` purescript
acquire :: forall m a. MonadResource m => Aff a -> (a -> Aff Unit) -> m (Tuple ReleaseKey a)
```

Given logic to acquire and free a resource `a`, this will: acquire the resource, register the release action, and
return the resource and the action's `ReleaseKey`

#### `deregister`

``` purescript
deregister :: forall m. MonadResource m => ReleaseKey -> m Unit
```

Remove the release action associated with the key. This will

#### `release`

``` purescript
release :: forall m. MonadResource m => ReleaseKey -> m Unit
```

Trigger the release action for this `ReleaseKey` if it is registered.

#### `release'`

``` purescript
release' :: forall m. MonadResource m => ReleaseKey -> m Boolean
```

Trigger the release action for this `ReleaseKey` if it is registered. Returns `true` if the key was found and the
action run, `false` otherwise.

#### `isRegistered`

``` purescript
isRegistered :: forall m. MonadResource m => ReleaseKey -> m Boolean
```

Returns `true` if the given `ReleaseKey` is registered in the current `ResourceT`, and `false` otherwise.

#### `isReleased`

``` purescript
isReleased :: forall m. MonadResource m => ReleaseKey -> m Boolean
```

Returns `true` if the given `ReleaseKey` is not registered in the current `ResourceT`, and `true` otherwise.

#### `fork`

``` purescript
fork :: forall a m. MonadResource m => Resource a -> m (Fiber a)
```

#### `forkAff`

``` purescript
forkAff :: forall a m. MonadResource m => Aff a -> m (Fiber a)
```

#### `supervise`

``` purescript
supervise :: forall a. Resource a -> Resource a
```


### Re-exported from Control.Monad.Resource.Class:

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

### Re-exported from Control.Monad.Resource.Internal.Registry:

#### `ReleaseKey`

``` purescript
newtype ReleaseKey
```

##### Instances
``` purescript
Eq ReleaseKey
Ord ReleaseKey
```

### Re-exported from Control.Monad.Resource.Trans:

#### `ResourceT`

``` purescript
newtype ResourceT m a
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

#### `mapResourceT`

``` purescript
mapResourceT :: forall m m' a b. (m a -> m' b) -> ResourceT m a -> ResourceT m' b
```

Change the type of the result in a `ResourceT` monad action.


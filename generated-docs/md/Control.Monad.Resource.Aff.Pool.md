## Module Control.Monad.Resource.Aff.Pool

#### `create`

``` purescript
create :: forall m a. MonadResource m => m (Aff a -> Aff a)
```

Create a function that will track all running Aff fibers and kill them in the resource cleanup.



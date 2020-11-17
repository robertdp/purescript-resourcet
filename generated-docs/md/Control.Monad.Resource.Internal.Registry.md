## Module Control.Monad.Resource.Internal.Registry

#### `Registry`

``` purescript
newtype Registry
  = Registry (Ref (Maybe { nextKey :: Int, references :: Int, releasers :: Map Int (Aff Unit) }))
```

#### `ReleaseKey`

``` purescript
newtype ReleaseKey
  = ReleaseKey Int
```

##### Instances
``` purescript
Eq ReleaseKey
Ord ReleaseKey
```

#### `register`

``` purescript
register :: Aff Unit -> Registry -> Effect ReleaseKey
```

#### `release`

``` purescript
release :: ReleaseKey -> Registry -> Aff Unit
```

#### `deregister`

``` purescript
deregister :: ReleaseKey -> Registry -> Effect Unit
```

#### `has`

``` purescript
has :: ReleaseKey -> Registry -> Effect Boolean
```

#### `reference`

``` purescript
reference :: Registry -> Aff Unit
```

#### `cleanup`

``` purescript
cleanup :: Registry -> Aff Unit
```

#### `releaseAll`

``` purescript
releaseAll :: Registry -> Aff (List Error)
```

#### `createEmpty`

``` purescript
createEmpty :: Effect Registry
```

#### `forkAff`

``` purescript
forkAff :: forall a. Aff a -> Registry -> Aff (Fiber a)
```



# ResourceT: Safe allocation and freeing of resources

`ResourceT` is a monad transformer that creates a region of code where you can safely allocate resources. These resources can then be freed manually on demand, or automatically at when either execution reaches the end of the region or an unrecovered error occurs within the region.

## Is this like `Aff.bracket`?

Yes, somewhat. But you can consider this the transformer version of `bracket` giving access to easier sequential composition. To this point `bracket` can be implemented using `ResourceT`, while the reverse is much more involved.

```purescript
import Control.Monad.Resource (class MonadResource, acquire, release)

bracket :: forall m a. MonadResource m => Aff a -> (a -> Aff Unit) -> (a -> m b) -> m b
bracket allocate free run = do
  key /\ resource <- acquire allocate free
  a <- run resource
  release key
  pure a
```

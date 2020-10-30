module Test.Main where

import Prelude
import Control.Monad.Resource (class MonadResource, ReleaseKey)
import Control.Monad.Resource as Resource
import Data.Array as Array
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_, parallel, sequential)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

data Token
  = One
  | Two
  | Three

derive instance eqToken :: Eq Token

instance showToken :: Show Token where
  show = case _ of
    One -> "One"
    Two -> "Two"
    Three -> "Three"

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "purescript-resourcet" do
          it "registers and releases resources in the expected order" do
            resource <- makeResource
            Resource.runResource do
              _ <- resource.register One
              _ <- resource.register Two
              _ <- resource.register Three
              liftAff do
                resource.expect.pending [ Tuple 0 One, Tuple 1 Two, Tuple 2 Three ]
                resource.expect.released []
            resource.expect.pending []
            resource.expect.released [ Three, Two, One ]
          it "doesn't re-release manually freed resources" do
            resource <- makeResource
            Resource.runResource do
              _ <- resource.register One
              key <- resource.register Two
              _ <- resource.register Three
              liftAff do
                resource.expect.pending [ Tuple 0 One, Tuple 1 Two, Tuple 2 Three ]
                resource.expect.released []
              Resource.release key
              liftAff do
                resource.expect.pending [ Tuple 0 One, Tuple 2 Three ]
                resource.expect.released [ Two ]
            resource.expect.pending []
            resource.expect.released [ Two, Three, One ]
          it "should handle parallel resource acquisition" do
            resource <- makeResource
            Resource.runResource do
              sequential ado
                parallel do
                  liftAff $ delay $ Milliseconds 500.0
                  _ <- resource.register One
                  pure unit
                parallel do
                  liftAff $ delay $ Milliseconds 200.0
                  _ <- resource.register Two
                  pure unit
                in unit
              _ <- resource.register Three
              liftAff do
                resource.expect.pending [ Tuple 0 Two, Tuple 1 One, Tuple 2 Three ]
                resource.expect.released []
            resource.expect.pending []
            resource.expect.released [ Three, One, Two ]
          it "should wait for forks to resolve before releasing" do
            resource <- makeResource
            Resource.runResource do
              _ <- resource.register One
              _ <- Resource.forkAff $ delay $ Milliseconds 500.0
              liftAff do
                resource.expect.pending [ Tuple 0 One ]
                resource.expect.released []
            resource.expect.pending [ Tuple 0 One ]
            resource.expect.released []
            delay $ Milliseconds 600.0
            resource.expect.pending []
            resource.expect.released [ One ]

makeResource ::
  forall m.
  MonadResource m =>
  Aff
    { expect ::
        { pending :: Array (Tuple Int Token) -> Aff Unit
        , released :: Array Token -> Aff Unit
        }
    , register :: Token -> m ReleaseKey
    }
makeResource =
  liftEffect do
    nextId <- Ref.new 0
    pending <- Ref.new Map.empty
    released <- Ref.new []
    let
      register token = do
        id <- liftEffect $ Ref.modify' (\s -> { state: s + 1, value: s }) nextId
        liftEffect $ Ref.modify_ (Map.insert id token) pending
        (Resource.register <<< liftEffect) do
          Ref.modify_ (Map.delete id) pending
          Ref.modify_ (flip Array.snoc token) released

      expect =
        { pending: \tokens -> liftEffect (Ref.read pending) >>= shouldEqual (Map.fromFoldable tokens)
        , released: \tokens -> liftEffect (Ref.read released) >>= shouldEqual tokens
        }
    pure { register, expect }

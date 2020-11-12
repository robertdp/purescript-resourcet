module Test.Main where

import Prelude
import Control.Monad.Resource (class MonadResource, ReleaseKey)
import Control.Monad.Resource as Resource
import Data.Array as Array
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), attempt, delay, error, forkAff, killFiber, launchAff_, parallel, sequential, throwError)
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
          it "handles parallel resource acquisition" do
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
          it "waits for all forks to resolve before freeing resources" do
            resource <- makeResource
            fork1 <- makeResource
            fork2 <- makeResource
            Resource.runResource do
              _ <- resource.register One
              _ <-
                Resource.fork do
                  liftAff $ delay $ Milliseconds 200.0
                  fork1.register Two
              _ <-
                Resource.fork do
                  _ <- fork2.register Three
                  liftAff $ delay $ Milliseconds 500.0
              liftAff do
                resource.expect.pending [ Tuple 0 One ]
                resource.expect.released []
                fork1.expect.pending []
                fork1.expect.released []
                fork2.expect.pending [ Tuple 0 Three ]
                fork2.expect.released []
            resource.expect.pending [ Tuple 0 One ]
            resource.expect.released []
            fork1.expect.pending []
            fork1.expect.released []
            fork2.expect.pending [ Tuple 0 Three ]
            fork2.expect.released []
            delay $ Milliseconds 250.0
            resource.expect.pending [ Tuple 0 One ]
            resource.expect.released []
            fork1.expect.pending []
            fork1.expect.released [ Two ]
            fork2.expect.pending [ Tuple 0 Three ]
            fork2.expect.released []
            delay $ Milliseconds 300.0
            resource.expect.pending []
            resource.expect.released [ One ]
            fork1.expect.pending []
            fork1.expect.released [ Two ]
            fork2.expect.pending []
            fork2.expect.released [ Three ]
          it "performs async cleanup when an error occurs" do
            finishedRelease <- liftEffect $ Ref.new false
            (liftEffect <<< launchAff_ <<< attempt <<< Resource.runResource) do
              _ <-
                Resource.register do
                  delay $ Milliseconds 50.0
                  liftEffect $ Ref.write true finishedRelease
              throwError (error "failing") $> unit
            delay $ Milliseconds 70.0
            wasSuccessful <- liftEffect $ Ref.read finishedRelease
            wasSuccessful `shouldEqual` true
          it "performs async cleanup when the fiber is killed" do
            finishedRelease <- liftEffect $ Ref.new false
            fiber <-
              (forkAff <<< Resource.runResource) do
                _ <-
                  Resource.register do
                    delay $ Milliseconds 50.0
                    liftEffect $ Ref.write true finishedRelease
                pure unit
            delay $ Milliseconds 10.0
            killFiber (error "killing") fiber
            delay $ Milliseconds 60.0
            wasSuccessful <- liftEffect $ Ref.read finishedRelease
            wasSuccessful `shouldEqual` true

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

{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "resourcet"
, dependencies =
  [ "aff"
  , "effect"
  , "foldable-traversable"
  , "maybe"
  , "ordered-collections"
  , "parallel"
  , "refs"
  , "tailrec"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

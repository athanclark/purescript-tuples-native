{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "tuples-native"
, dependencies =
  [ "console"
  , "effect"
  , "functions"
  , "prelude"
  , "psci-support"
  , "tuples"
  , "typelevel"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

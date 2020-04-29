{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "tuples-native"
, dependencies =
  [ "generics-rep"
  , "prelude"
  , "psci-support"
  , "typelevel"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

module Test.Main where

import Data.Tuple.Native
import Data.Typelevel.Num

import Prelude
import Effect (Effect)
import Effect.Console (logShow)

main :: Effect Unit
main = do
  let x :: T3 Int Int Int
      x = t3 1 2 3

  logShow $ prj d0 x
  logShow $ prj d1 x
  logShow $ prj d2 x

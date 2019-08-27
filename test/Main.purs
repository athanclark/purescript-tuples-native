module Test.Main where

import Data.Tuple.Native (T3, prj, t3)
import Data.Typelevel.Num (d0, d1, d2)

import Prelude (Unit, discard, ($))
import Effect (Effect)
import Effect.Console (logShow)

main :: Effect Unit
main = do
  let x :: T3 Int Int Int
      x = t3 1 2 3

  logShow $ prj d0 x
  logShow $ prj d1 x
  logShow $ prj d2 x

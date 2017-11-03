module Test.Main where

import Data.Tuple.Native
import Data.Typelevel.Num

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let x = t3 1 2 3

  logShow $ prj d0 x
  logShow $ prj d1 x
  logShow $ prj d2 x

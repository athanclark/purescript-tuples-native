module Test.Main where

import Data.Tuple.Native (T2, T3, prj, t3, xt, xt3)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (Tuple3)
import Data.Typelevel.Num (d0, d1, d2)

import Prelude (Unit, discard, ($))
import Effect (Effect)
import Effect.Console (logShow)

foreign import lenTupleImpl :: String -> T2 String Int

lenTuple :: String -> Tuple String Int
lenTuple s = xt $ lenTupleImpl s

main :: Effect Unit
main = do
  let x :: T3 Int Int Int
      x = t3 1 2 3

  logShow $ prj d0 x
  logShow $ prj d1 x
  logShow $ prj d2 x

  let t :: Tuple3 Int Int Int
      t = xt3 x
  logShow $ t

  -- test foreign import
  let l :: Tuple String Int
      l = lenTuple "Testing"
  logShow l

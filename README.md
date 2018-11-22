# purescript-tuples-native

This library defines tuple data types that are represented under-the-hood as heterogeneous arrays to JavaScript - useful for
foreign interfaces:

```purescript
import Data.Tuple.Native (T3, t3, prj)
import Data.TypeLevel.Num.Reps (d0, d1, d2)

xs :: T3 String Int Boolean
xs = t3 "foo" 13 false

prj d0 xs == "foo" -- true
prj d1 xs == 13 -- true
prj d2 xs == false -- true
```

# purescript-tuples-native

This library defines tuple data types that are represented under-the-hood as heterogeneous arrays to JavaScript - useful for
foreign interfaces. A set of `xt` functions are exposed to translate these native Tuples into Purescript pairs
or Nested tuples. So for example:

```purescript
-- Main.purs
import Data.Tuple.Native (T2, xt)
import Data.Tuple (Tuple)

foreign import lenTupleImpl :: String -> T2 String Int

lenTuple :: String -> Tuple String Int
lenTuple s = xt $ lenTupleImpl s
```

Could let you wrap this Javascript function
```javascript
"use strict";
function lenTuple (string) {
    return [string, string.length]
}
exports.lenTupleImpl = lenTuple
```

You can also extract indidual elements directly from the Native tuple using `prj`.

```purescript
import Data.Tuple.Native (T3, t3, prj)
import Data.TypeLevel.Num.Reps (d0, d1, d2)

xs :: T3 String Int Boolean
xs = t3 "foo" 13 false

prj d0 xs == "foo" -- true
prj d1 xs == 13 -- true
prj d2 xs == false -- true
```

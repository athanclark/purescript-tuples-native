-- | Heterogeneous arrays for foreign function interfaces.

module Data.Tuple.Native
  ( TupleN
  , T2, T3, T4, T5, T6, T7, T8, T9
  , t2, t3, t4, t5, t6, t7, t8, t9
  , t2_, t3_, t4_, t5_, t6_, t7_, t8_, t9_
  , xt
  , xt2, xt3, xt4, xt5, xt6, xt7, xt8, xt9
  , prj
  , class TupleSize, class ShowNat
  ) where

import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn5, Fn6, Fn7, Fn8, Fn9, runFn2, runFn3, runFn4, runFn5, runFn6, runFn7, runFn8, runFn9)
import Data.Generic.Rep (class Generic, Constructor(..), Argument(..), Product(..))
import Data.Tuple as DT
import Data.Tuple.Nested as DTN
import Data.Typelevel.Num (D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, class Lt, class Nat, toInt, d0, d1, d2, d3, d4, d5, d6, d7, d8)
import Prelude (($))
import Prim.Row (class Cons)
import Type.RowList (Cons, Nil, RowList, class ListToRow)

-- | Safely coerce a `TupleN` pair into a PureScript Tuple
xt :: forall a b. T2 a b -> DT.Tuple a b
xt t = DT.Tuple (prj d0 t) (prj d1 t)

-- | Safely coerce a `TupleN` pair into a PureScript Nested Tuple
xt2 :: forall a b. T2 a b -> DTN.Tuple2 a b
xt2 t = DTN.tuple2 (prj d0 t) (prj d1 t)

xt3 :: forall a b c. T3 a b c -> DTN.Tuple3 a b c
xt3 t = DTN.tuple3 (prj d0 t) (prj d1 t) (prj d2 t)

xt4 :: forall a b c d. T4 a b c d -> DTN.Tuple4 a b c d
xt4 t = DTN.tuple4 (prj d0 t) (prj d1 t) (prj d2 t) (prj d3 t)

xt5 :: forall a b c d e. T5 a b c d e -> DTN.Tuple5 a b c d e
xt5 t = DTN.tuple5 (prj d0 t) (prj d1 t) (prj d2 t) (prj d3 t) (prj d4 t)

xt6 :: forall a b c d e f. T6 a b c d e f -> DTN.Tuple6 a b c d e f
xt6 t = DTN.tuple6 (prj d0 t) (prj d1 t) (prj d2 t) (prj d3 t) (prj d4 t) (prj d5 t)

xt7 :: forall a b c d e f g. T7 a b c d e f g -> DTN.Tuple7 a b c d e f g
xt7 t = DTN.tuple7 (prj d0 t) (prj d1 t) (prj d2 t) (prj d3 t) (prj d4 t) (prj d5 t) (prj d6 t)

xt8 :: forall a b c d e f g h. T8 a b c d e f g h -> DTN.Tuple8 a b c d e f g h
xt8 t = DTN.tuple8 (prj d0 t) (prj d1 t) (prj d2 t) (prj d3 t) (prj d4 t) (prj d5 t) (prj d6 t) (prj d7 t)

xt9 :: forall a b c d e f g h i. T9 a b c d e f g h i -> DTN.Tuple9 a b c d e f g h i
xt9 t = DTN.tuple9 (prj d0 t) (prj d1 t) (prj d2 t) (prj d3 t) (prj d4 t) (prj d5 t) (prj d6 t) (prj d7 t) (prj d8 t)

foreign import prjImpl :: forall t a. Fn2 Int (TupleN t) a

-- | Project a value of index `n` from a `TupleN`, using `Data.TypeLevel.Num.Reps.dN` as Nat values.
prj :: forall t t' t'' n n' a size
     . TupleSize size t
    => Lt n size
    => ShowNat n n'
    => ListToRow t t'
    => Cons n' a t'' t'
    => Nat n
    => n -> TupleN t -> a
prj n t = runFn2 prjImpl (toInt n) t

-- | Represented as a heterogeneous array under the hood
foreign import data TupleN :: RowList Type -> Type

type T2  a b =
  TupleN (Cons "0" a (Cons "1" b Nil))
type T3  a b c =
  TupleN (Cons "0" a (Cons "1" b (Cons "2" c Nil)))
type T4  a b c d =
  TupleN (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d Nil))))
type T5  a b c d e =
  TupleN (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e Nil)))))
type T6  a b c d e f =
  TupleN (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e (Cons "5" f Nil))))))
type T7  a b c d e f g =
  TupleN (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e (Cons "5" f (Cons "6" g Nil)))))))
type T8  a b c d e f g h =
  TupleN (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e (Cons "5" f (Cons "6" g (Cons "7" h Nil))))))))
type T9  a b c d e f g h i =
  TupleN (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e (Cons "5" f (Cons "6" g (Cons "7" h (Cons "8" i Nil)))))))))


t2 :: forall a b              . a -> b -> T2 a b
t2 = runFn2 t2_
t3 :: forall a b c            . a -> b -> c -> (T3 a b c)
t3 = runFn3 t3_
t4 :: forall a b c d          . a -> b -> c -> d -> (T4 a b c d)
t4 = runFn4 t4_
t5 :: forall a b c d e        . a -> b -> c -> d -> e -> (T5 a b c d e)
t5 = runFn5 t5_
t6 :: forall a b c d e f      . a -> b -> c -> d -> e -> f -> (T6 a b c d e f)
t6 = runFn6 t6_
t7 :: forall a b c d e f g    . a -> b -> c -> d -> e -> f -> g -> (T7 a b c d e f g)
t7 = runFn7 t7_
t8 :: forall a b c d e f g h  . a -> b -> c -> d -> e -> f -> g -> h -> (T8 a b c d e f g h)
t8 = runFn8 t8_
t9 :: forall a b c d e f g h i. a -> b -> c -> d -> e -> f -> g -> h -> i -> (T9 a b c d e f g h i)
t9 = runFn9 t9_

foreign import t2_ :: forall a b              . Fn2 a b (T2 a b)
foreign import t3_ :: forall a b c            . Fn3 a b c (T3 a b c)
foreign import t4_ :: forall a b c d          . Fn4 a b c d (T4 a b c d)
foreign import t5_ :: forall a b c d e        . Fn5 a b c d e (T5 a b c d e)
foreign import t6_ :: forall a b c d e f      . Fn6 a b c d e f (T6 a b c d e f)
foreign import t7_ :: forall a b c d e f g    . Fn7 a b c d e f g (T7 a b c d e f g)
foreign import t8_ :: forall a b c d e f g h  . Fn8 a b c d e f g h (T8 a b c d e f g h)
foreign import t9_ :: forall a b c d e f g h i. Fn9 a b c d e f g h i (T9 a b c d e f g h i)

class TupleSize :: forall k. k -> RowList Type -> Constraint
class TupleSize n (t :: RowList Type) | t -> n

instance TupleSize D2 (Cons "0" a (Cons "1" b Nil))
instance TupleSize D3 (Cons "0" a (Cons "1" b (Cons "2" c Nil)))
instance TupleSize D4 (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d Nil))))
instance TupleSize D5 (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e Nil)))))
instance TupleSize D6 (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e (Cons "5" f Nil))))))
instance TupleSize D7 (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e (Cons "5" f (Cons "6" g Nil)))))))
instance TupleSize D8 (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e (Cons "5" f (Cons "6" g (Cons "7" h Nil))))))))
instance TupleSize D9 (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e (Cons "5" f (Cons "6" g (Cons "7" h (Cons "8" i Nil)))))))))


instance Generic
         (TupleN (Cons "0" a (Cons "1" b Nil)))
         (Constructor "t2" (Product (Argument a) (Argument b))) where
  to (Constructor (Product (Argument a) (Argument b))) = runFn2 t2_ a b
  from xs = Constructor (Product (Argument (prj d0 xs)) (Argument (prj d1 xs)))

instance Generic
         (TupleN (Cons "0" a (Cons "1" b (Cons "2" c Nil))))
         (Constructor "t3" (Product (Argument a) (Product (Argument b) (Argument c)))) where
  to (Constructor (Product (Argument a) (Product (Argument b) (Argument c)))) = runFn3 t3_ a b c
  from xs = Constructor $ Product (Argument (prj d0 xs)) $ Product (Argument (prj d1 xs)) (Argument (prj d2 xs))

instance Generic
         (TupleN (Cons "0" a
                 (Cons "1" b
                 (Cons "2" c
                 (Cons "3" d Nil)))))
         (Constructor "t4" (Product (Argument a)
                           (Product (Argument b)
                           (Product (Argument c)
                                    (Argument d))))) where
  to (Constructor (Product (Argument a)
                  (Product (Argument b)
                  (Product (Argument c)
                           (Argument d))))) = runFn4 t4_ a b c d
  from xs = Constructor $ Product (Argument (prj d0 xs))
                        $ Product (Argument (prj d1 xs))
                        $ Product (Argument (prj d2 xs))
                                  (Argument (prj d3 xs))

instance Generic
         (TupleN (Cons "0" a
                 (Cons "1" b
                 (Cons "2" c
                 (Cons "3" d
                 (Cons "4" e Nil))))))
         (Constructor "t5" (Product (Argument a)
                           (Product (Argument b)
                           (Product (Argument c)
                           (Product (Argument d)
                                    (Argument e)))))) where
  to (Constructor (Product (Argument a)
                  (Product (Argument b)
                  (Product (Argument c)
                  (Product (Argument d)
                           (Argument e)))))) = runFn5 t5_ a b c d e
  from xs = Constructor $ Product (Argument (prj d0 xs))
                        $ Product (Argument (prj d1 xs))
                        $ Product (Argument (prj d2 xs))
                        $ Product (Argument (prj d3 xs))
                                  (Argument (prj d4 xs))

instance Generic
         (TupleN (Cons "0" a
                 (Cons "1" b
                 (Cons "2" c
                 (Cons "3" d
                 (Cons "4" e
                 (Cons "5" f Nil)))))))
         (Constructor "t6" (Product (Argument a)
                           (Product (Argument b)
                           (Product (Argument c)
                           (Product (Argument d)
                           (Product (Argument e)
                                    (Argument f))))))) where
  to (Constructor (Product (Argument a)
                  (Product (Argument b)
                  (Product (Argument c)
                  (Product (Argument d)
                  (Product (Argument e)
                           (Argument f))))))) = runFn6 t6_ a b c d e f
  from xs = Constructor $ Product (Argument (prj d0 xs))
                        $ Product (Argument (prj d1 xs))
                        $ Product (Argument (prj d2 xs))
                        $ Product (Argument (prj d3 xs))
                        $ Product (Argument (prj d4 xs))
                                  (Argument (prj d5 xs))

instance Generic
         (TupleN (Cons "0" a
                 (Cons "1" b
                 (Cons "2" c
                 (Cons "3" d
                 (Cons "4" e
                 (Cons "5" f
                 (Cons "6" g Nil))))))))
         (Constructor "t7" (Product (Argument a)
                           (Product (Argument b)
                           (Product (Argument c)
                           (Product (Argument d)
                           (Product (Argument e)
                           (Product (Argument f)
                                    (Argument g)))))))) where
  to (Constructor (Product (Argument a)
                  (Product (Argument b)
                  (Product (Argument c)
                  (Product (Argument d)
                  (Product (Argument e)
                  (Product (Argument f)
                           (Argument g)))))))) = runFn7 t7_ a b c d e f g
  from xs = Constructor $ Product (Argument (prj d0 xs))
                        $ Product (Argument (prj d1 xs))
                        $ Product (Argument (prj d2 xs))
                        $ Product (Argument (prj d3 xs))
                        $ Product (Argument (prj d4 xs))
                        $ Product (Argument (prj d5 xs))
                                  (Argument (prj d6 xs))

instance Generic
         (TupleN (Cons "0" a
                 (Cons "1" b
                 (Cons "2" c
                 (Cons "3" d
                 (Cons "4" e
                 (Cons "5" f
                 (Cons "6" g
                 (Cons "7" h Nil)))))))))
         (Constructor "t8" (Product (Argument a)
                           (Product (Argument b)
                           (Product (Argument c)
                           (Product (Argument d)
                           (Product (Argument e)
                           (Product (Argument f)
                           (Product (Argument g)
                                    (Argument h))))))))) where
  to (Constructor (Product (Argument a)
                  (Product (Argument b)
                  (Product (Argument c)
                  (Product (Argument d)
                  (Product (Argument e)
                  (Product (Argument f)
                  (Product (Argument g)
                           (Argument h))))))))) = runFn8 t8_ a b c d e f g h
  from xs = Constructor $ Product (Argument (prj d0 xs))
                        $ Product (Argument (prj d1 xs))
                        $ Product (Argument (prj d2 xs))
                        $ Product (Argument (prj d3 xs))
                        $ Product (Argument (prj d4 xs))
                        $ Product (Argument (prj d5 xs))
                        $ Product (Argument (prj d6 xs))
                                  (Argument (prj d7 xs))

instance Generic
         (TupleN (Cons "0" a
                 (Cons "1" b
                 (Cons "2" c
                 (Cons "3" d
                 (Cons "4" e
                 (Cons "5" f
                 (Cons "6" g
                 (Cons "7" h
                 (Cons "8" i Nil))))))))))
         (Constructor "t9" (Product (Argument a)
                           (Product (Argument b)
                           (Product (Argument c)
                           (Product (Argument d)
                           (Product (Argument e)
                           (Product (Argument f)
                           (Product (Argument g)
                           (Product (Argument h)
                                    (Argument i)))))))))) where
  to (Constructor (Product (Argument a)
                  (Product (Argument b)
                  (Product (Argument c)
                  (Product (Argument d)
                  (Product (Argument e)
                  (Product (Argument f)
                  (Product (Argument g)
                  (Product (Argument h)
                           (Argument i)))))))))) = runFn9 t9_ a b c d e f g h i
  from xs = Constructor $ Product (Argument (prj d0 xs))
                        $ Product (Argument (prj d1 xs))
                        $ Product (Argument (prj d2 xs))
                        $ Product (Argument (prj d3 xs))
                        $ Product (Argument (prj d4 xs))
                        $ Product (Argument (prj d5 xs))
                        $ Product (Argument (prj d6 xs))
                        $ Product (Argument (prj d7 xs))
                                  (Argument (prj d8 xs))


class ShowNat n (s :: Symbol) | n -> s, s -> n

instance ShowNat D0 "0"
instance ShowNat D1 "1"
instance ShowNat D2 "2"
instance ShowNat D3 "3"
instance ShowNat D4 "4"
instance ShowNat D5 "5"
instance ShowNat D6 "6"
instance ShowNat D7 "7"
instance ShowNat D8 "8"

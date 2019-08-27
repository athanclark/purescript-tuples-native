-- | Heterogeneous arrays for foreign function interfaces.

module Data.Tuple.Native
  ( TupleN
  , T2, T3, T4, T5, T6, T7, T8, T9
  , t2, t3, t4, t5, t6, t7, t8, t9
  , prj
  , class TupleSize, class ShowNat
  ) where

import Prelude (($))
import Data.Typelevel.Num
  ( D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, class Lt, class Nat, toInt
  , d0, d1, d2, d3, d4, d5, d6, d7, d8)
import Data.Generic.Rep (class Generic, Constructor (..), Argument (..), Product (..))
import Type.RowList (Cons, Nil, kind RowList, class ListToRow)
import Prim.Row (class Cons)


foreign import prjImpl :: forall t a. Int -> TupleN t -> a

-- | Project a value of index `n` from a `TupleN`, using `Data.TypeLevel.Num.Reps.dN` as Nat values.
prj :: forall t t' t'' n n' a size
     . TupleSize size t
    => Lt n size
    => ShowNat n n'
    => ListToRow t t'
    => Cons n' a t'' t'
    => Nat n
    => n -> TupleN t -> a
prj n t = prjImpl (toInt n) t



-- | Represented as a heterogeneous array under the hood
foreign import data TupleN :: RowList -> Type


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

foreign import t2 :: forall a b              . a -> b -> T2 a b
foreign import t3 :: forall a b c            . a -> b -> c -> T3 a b c
foreign import t4 :: forall a b c d          . a -> b -> c -> d -> T4 a b c d
foreign import t5 :: forall a b c d e        . a -> b -> c -> d -> e -> T5 a b c d e
foreign import t6 :: forall a b c d e f      . a -> b -> c -> d -> e -> f -> T6 a b c d e f
foreign import t7 :: forall a b c d e f g    . a -> b -> c -> d -> e -> f -> g -> T7 a b c d e f g
foreign import t8 :: forall a b c d e f g h  . a -> b -> c -> d -> e -> f -> g -> h -> T8 a b c d e f g h
foreign import t9 :: forall a b c d e f g h i. a -> b -> c -> d -> e -> f -> g -> h -> i -> T9 a b c d e f g h i

class TupleSize n (t :: RowList) | t -> n

instance tupleSizeT2 :: TupleSize D2 (Cons "0" a (Cons "1" b Nil))
instance tupleSizeT3 :: TupleSize D3 (Cons "0" a (Cons "1" b (Cons "2" c Nil)))
instance tupleSizeT4 :: TupleSize D4 (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d Nil))))
instance tupleSizeT5 :: TupleSize D5 (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e Nil)))))
instance tupleSizeT6 :: TupleSize D6 (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e (Cons "5" f Nil))))))
instance tupleSizeT7 :: TupleSize D7 (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e (Cons "5" f (Cons "6" g Nil)))))))
instance tupleSizeT8 :: TupleSize D8 (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e (Cons "5" f (Cons "6" g (Cons "7" h Nil))))))))
instance tupleSizeT9 :: TupleSize D9 (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e (Cons "5" f (Cons "6" g (Cons "7" h (Cons "8" i Nil)))))))))


instance genericTuple2 :: Generic
         (TupleN (Cons "0" a (Cons "1" b Nil)))
         (Constructor "t2" (Product (Argument a) (Argument b))) where
  to (Constructor (Product (Argument a) (Argument b))) = t2 a b
  from xs = Constructor (Product (Argument (prj d0 xs)) (Argument (prj d1 xs)))

instance genericTuple3 :: Generic
         (TupleN (Cons "0" a (Cons "1" b (Cons "2" c Nil))))
         (Constructor "t3" (Product (Argument a) (Product (Argument b) (Argument c)))) where
  to (Constructor (Product (Argument a) (Product (Argument b) (Argument c)))) = t3 a b c
  from xs = Constructor $ Product (Argument (prj d0 xs)) $ Product (Argument (prj d1 xs)) (Argument (prj d2 xs))

instance genericTuple4 :: Generic
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
                           (Argument d))))) = t4 a b c d
  from xs = Constructor $ Product (Argument (prj d0 xs))
                        $ Product (Argument (prj d1 xs))
                        $ Product (Argument (prj d2 xs))
                                  (Argument (prj d3 xs))

instance genericTuple5 :: Generic
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
                           (Argument e)))))) = t5 a b c d e
  from xs = Constructor $ Product (Argument (prj d0 xs))
                        $ Product (Argument (prj d1 xs))
                        $ Product (Argument (prj d2 xs))
                        $ Product (Argument (prj d3 xs))
                                  (Argument (prj d4 xs))

instance genericTuple6 :: Generic
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
                           (Argument f))))))) = t6 a b c d e f
  from xs = Constructor $ Product (Argument (prj d0 xs))
                        $ Product (Argument (prj d1 xs))
                        $ Product (Argument (prj d2 xs))
                        $ Product (Argument (prj d3 xs))
                        $ Product (Argument (prj d4 xs))
                                  (Argument (prj d5 xs))

instance genericTuple7 :: Generic
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
                           (Argument g)))))))) = t7 a b c d e f g
  from xs = Constructor $ Product (Argument (prj d0 xs))
                        $ Product (Argument (prj d1 xs))
                        $ Product (Argument (prj d2 xs))
                        $ Product (Argument (prj d3 xs))
                        $ Product (Argument (prj d4 xs))
                        $ Product (Argument (prj d5 xs))
                                  (Argument (prj d6 xs))

instance genericTuple8 :: Generic
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
                           (Argument h))))))))) = t8 a b c d e f g h
  from xs = Constructor $ Product (Argument (prj d0 xs))
                        $ Product (Argument (prj d1 xs))
                        $ Product (Argument (prj d2 xs))
                        $ Product (Argument (prj d3 xs))
                        $ Product (Argument (prj d4 xs))
                        $ Product (Argument (prj d5 xs))
                        $ Product (Argument (prj d6 xs))
                                  (Argument (prj d7 xs))

instance genericTuple9 :: Generic
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
                           (Argument i)))))))))) = t9 a b c d e f g h i
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

instance showNatD0 :: ShowNat D0 "0"
instance showNatD1 :: ShowNat D1 "1"
instance showNatD2 :: ShowNat D2 "2"
instance showNatD3 :: ShowNat D3 "3"
instance showNatD4 :: ShowNat D4 "4"
instance showNatD5 :: ShowNat D5 "5"
instance showNatD6 :: ShowNat D6 "6"
instance showNatD7 :: ShowNat D7 "7"
instance showNatD8 :: ShowNat D8 "8"

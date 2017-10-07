module Data.Tuple.Native
  ( TupleN
  , T2, T3, T4, T5, T6, T7, T8, T9
  , t2, t3, t4, t5, t6, t7, t8, t9
  , prj
  , class TupleSize, class ShowNat
  ) where

import Prelude (class Show)
import Data.Typelevel.Num (D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, class Lt, class Nat, toInt)
import Type.Row (class RowToList, Cons, Nil)


foreign import prjImpl :: forall t a. Int -> TupleN t -> a

prj :: forall t t' n n' a size
     . TupleSize t size
    => Lt n size
    => ShowNat n n'
    => RowCons n' a t' t
    => Nat n
    => n -> TupleN t -> a
prj n t = prjImpl (toInt n) t


-- | Represented as a heterogeneous array under the hood
foreign import data TupleN :: # Type -> Type

foreign import showTupleN :: forall t. TupleN t -> String

-- | Unsafe show instance - expects all entries to have a show instance
instance showTupleNInst :: Show (TupleN t) where
  show = showTupleN

type T2  a b =
  TupleN ("0" :: a, "1" :: b)
type T3  a b c =
  TupleN ("0" :: a, "1" :: b, "2" :: c)
type T4  a b c d =
  TupleN ("0" :: a, "1" :: b, "2" :: c, "3" :: d)
type T5  a b c d e =
  TupleN ("0" :: a, "1" :: b, "2" :: c, "3" :: d, "4" :: e)
type T6  a b c d e f =
  TupleN ("0" :: a, "1" :: b, "2" :: c, "3" :: d, "4" :: e, "5" :: f)
type T7  a b c d e f g =
  TupleN ("0" :: a, "1" :: b, "2" :: c, "3" :: d, "4" :: e, "5" :: f, "6" :: g)
type T8  a b c d e f g h =
  TupleN ("0" :: a, "1" :: b, "2" :: c, "3" :: d, "4" :: e, "5" :: f, "6" :: g, "7" :: h)
type T9  a b c d e f g h i =
  TupleN ("0" :: a, "1" :: b, "2" :: c, "3" :: d, "4" :: e, "5" :: f, "6" :: g, "7" :: h, "8" :: i)

foreign import t2 :: forall a b              . a -> b -> T2 a b
foreign import t3 :: forall a b c            . a -> b -> c -> T3 a b c
foreign import t4 :: forall a b c d          . a -> b -> c -> d -> T4 a b c d
foreign import t5 :: forall a b c d e        . a -> b -> c -> d -> e -> T5 a b c d e
foreign import t6 :: forall a b c d e f      . a -> b -> c -> d -> e -> f -> T6 a b c d e f
foreign import t7 :: forall a b c d e f g    . a -> b -> c -> d -> e -> f -> g -> T7 a b c d e f g
foreign import t8 :: forall a b c d e f g h  . a -> b -> c -> d -> e -> f -> g -> h -> T8 a b c d e f g h
foreign import t9 :: forall a b c d e f g h i. a -> b -> c -> d -> e -> f -> g -> h -> i -> T9 a b c d e f g h i

class TupleSize (t :: # Type) n | t -> n

instance tupleSizeT2 :: RowToList t (Cons "0" a (Cons "1" b Nil))
                     => TupleSize t D2
instance tupleSizeT3 :: RowToList t (Cons "0" a (Cons "1" b (Cons "2" c Nil)))
                     => TupleSize t D3
instance tupleSizeT4 :: RowToList t (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d Nil))))
                     => TupleSize t D4
instance tupleSizeT5 :: RowToList t (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e Nil)))))
                     => TupleSize t D5
instance tupleSizeT6 :: RowToList t (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e (Cons "5" f Nil))))))
                     => TupleSize t D6
instance tupleSizeT7 :: RowToList t (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e (Cons "5" f (Cons "6" g Nil)))))))
                     => TupleSize t D7
instance tupleSizeT8 :: RowToList t (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e (Cons "5" f (Cons "6" g (Cons "7" h Nil))))))))
                     => TupleSize t D8
instance tupleSizeT9 :: RowToList t (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e (Cons "5" f (Cons "6" g (Cons "7" h (Cons "8" i Nil)))))))))
                     => TupleSize t D9



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
instance showNatD9 :: ShowNat D9 "9"

module Data.Tuple.Native
  ( TupleN
  , T2, T3, T4, T5, T6, T7, T8, T9
  , t2, t3, t4, t5, t6, t7, t8, t9
  , prj
  , class TupleSize, class ShowNat
  -- , class Over, over
  ) where

import Prelude (class Show)
import Data.Typelevel.Num (D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, class Lt, class Nat, toInt)
-- import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Type.Row (class RowToList, class ListToRow, Cons, Nil, kind RowList)
-- import Data.Symbol (kind Symbol)
import Unsafe.Coerce (unsafeCoerce)


foreign import prjImpl :: forall t a. Int -> TupleN t -> a

prj :: forall t t' t_ n n' a size
     . RowToList t t_
    => TupleSize size t_
    => Lt n size
    => ShowNat n n'
    => RowCons n' a t' t
    => Nat n
    => n -> TupleN t -> a
prj n t = prjImpl (toInt n) t


-- foreign import overImpl :: forall t p a b. Int -> (a -> b) -> TupleN t -> TupleN p


-- class Nat n <= Over n (a :: Type) (b :: Type) (t :: # Type) (p :: # Type) | n b t -> p a, n a p -> t b

-- instance overNil :: (Nat n, RowToList t Nil, ListToRow Nil p) => Over n a b t p
-- instance overCons :: ( ShowNat n n'
--                      , Over n a b ts ps
--                      , RowToList ts ts'
--                      , RowToList t (Cons n' a ts')
--                      , ListToRow ps' ps
--                      , ListToRow (Cons n' b ps') p
--                      ) => Over n a b t p
-- instance overCons' :: ( ShowNat n n'
--                       , Over n a b ts ps
--                       , RowToList ts ts'
--                       , RowToList t (Cons m' a ts')
--                       , ListToRow ps' ps
--                       , ListToRow (Cons m' b ps') p
--                       ) => Over n a b t p

-- over :: forall size n n' a b t t' p
--       . TupleSize t size
--      => Lt n size
--      => RowCons n' a t' t
--      => ShowNat n n'
--      => Over n a b t p
--      => Nat n
--      => n -> (a -> b) -> TupleN t -> TupleN p
-- over n f x = overImpl (toInt n) f x



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

class TupleSize n (t :: RowList) | t -> n

instance tupleSizeT2 :: TupleSize D2 (Cons "0" a (Cons "1" b Nil))
instance tupleSizeT3 :: TupleSize D3 (Cons "0" a (Cons "1" b (Cons "2" c Nil)))
instance tupleSizeT4 :: TupleSize D4 (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d Nil))))
instance tupleSizeT5 :: TupleSize D5 (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e Nil)))))
instance tupleSizeT6 :: TupleSize D6 (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e (Cons "5" f Nil))))))
instance tupleSizeT7 :: TupleSize D7 (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e (Cons "5" f (Cons "6" g Nil)))))))
instance tupleSizeT8 :: TupleSize D8 (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e (Cons "5" f (Cons "6" g (Cons "7" h Nil))))))))
instance tupleSizeT9 :: TupleSize D9 (Cons "0" a (Cons "1" b (Cons "2" c (Cons "3" d (Cons "4" e (Cons "5" f (Cons "6" g (Cons "7" h (Cons "8" i Nil)))))))))



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

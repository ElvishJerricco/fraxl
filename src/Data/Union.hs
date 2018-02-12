{-# LANGUAGE EmptyDataDecls #-}
module Data.Union where

import Control.Lens
import Data.GADT.Compare

-- | 'FunctorCoRec' doesn't implement 'GCompare'.
-- To avoid orphan instances, a newtype is defined.
--
-- @Union@ represents a value of any type constructor in @r@ applied with @a@.
data family Union (r :: [* -> *]) (a :: *)
data instance Union '[] a
data instance Union (f : fs) a = UnionLeft (f a)
                               | UnionRight (Union fs a)


class In1 (f :: k -> *) (fs :: k -> *) where
  accessor :: Prism' (fs a) (f a)

instance {-# OVERLAPPING #-} In1 f (Union (f : fs)) where
  accessor = prism' UnionLeft (\ case UnionLeft a -> Just a
                                      UnionRight _ -> Nothing)

instance {-# OVERLAPPABLE #-} In1 f (Union fs) => In1 f (Union (g : fs)) where
  accessor = prism' (\ x -> UnionRight $ x ^. re accessor)
                    (\ case UnionLeft _ -> Nothing
                            UnionRight a -> a ^? accessor)

type f ∈ fs = In1 f fs


instance GEq (Union '[]) where
  a `geq` _ = case a of

instance (GEq f, GEq (Union r)) => GEq (Union (f ': r)) where
  UnionLeft  a `geq` UnionLeft  b = a `geq` b
  UnionLeft  _ `geq` UnionRight _ = Nothing
  UnionRight _ `geq` UnionLeft  _ = Nothing
  UnionRight a `geq` UnionRight b = a `geq` b

instance GCompare (Union '[]) where
  a `gcompare` _ = case a of

instance (GCompare f, GCompare (Union r)) => GCompare (Union (f ': r)) where
  UnionLeft  a `gcompare` UnionLeft  b = a `gcompare` b
  UnionLeft  _ `gcompare` UnionRight _ = GLT
  UnionRight _ `gcompare` UnionLeft  _ = GGT
  UnionRight a `gcompare` UnionRight b = a `gcompare` b

module Data.Union where

import Control.Lens

-- | 'FunctorCoRec' doesn't implement 'GCompare'.
-- To avoid orphan instances, a newtype is defined.
--
-- @Union@ represents a value of any type constructor in @r@ applied with @a@.
data family Union (r :: [* -> *]) (a :: *)
data instance Union '[] a = UnionNil
data instance Union (f : fs) a = UnionLeft (f a)
                               | UnionRight (Union fs a)


class In1 (f :: k -> *) (fs :: k -> *) where
  accessor :: Prism' (fs a) (f a)

instance In1 f (Union (f : fs)) where
  accessor = prism' UnionLeft (\ case UnionLeft a -> Just a
                                      UnionRight _ -> Nothing)

instance In1 f (Union fs) => In1 f (Union (g : fs)) where
  accessor = prism' (\ x -> UnionRight $ x ^. re accessor)
                    (\ case UnionLeft _ -> Nothing
                            UnionRight a -> a ^? accessor)

type f ∈ fs = In1 f fs

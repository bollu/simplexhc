{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
module OrderedMap(OrderedMap,
  fromList,
  size,
  adjust,
  insert,
  elems,
  toList,
  keys,
  (!),
  union,
  OrderedMap.lookup) where
import qualified Data.Map.Strict as M
import Data.Monoid
import ColorUtils
import Data.Text.Prettyprint.Doc
import qualified Data.List as L

-- At some point, I need this. This is more convenient than overloading the key to store the insertion time.
-- | A dictionary that orders elements by insertion time
data OrderedMap k v = OrderedMap { map' :: M.Map k v, order :: [k] } deriving(Show, Functor, Foldable, Traversable)

instance (Ord k, Pretty k, Pretty v) => Pretty (OrderedMap k v) where
  pretty ok = vcat (map pkv (toList ok)) where
    pkv :: (Pretty k, Pretty v) => (k, v) -> Doc ann
    pkv (k, v) = pretty "** key: " <+> pretty k <+> pretty " | value : " <+> pretty v

instance Ord k => Monoid (OrderedMap k v) where
  mempty :: OrderedMap k v
  mempty = OrderedMap mempty mempty

  mappend :: OrderedMap k v -> OrderedMap k v -> OrderedMap k v
  mappend (OrderedMap m o) (OrderedMap m' o') = OrderedMap (m `mappend` m') (o `mappend` o')

liftMapEdit_ :: (M.Map k v -> M.Map k v) -> OrderedMap k v -> OrderedMap k v
liftMapEdit_ f (OrderedMap map' order) = OrderedMap (f map') order

liftMapExtract_ :: (M.Map k v -> a) -> OrderedMap k v -> a
liftMapExtract_ f (OrderedMap map' _) = f map'

-- | NOTE: this will maintain the order of insertion. Elements that are inserted
-- | later are returned later in the `keys`, `elems`.
insert  :: Ord k => k -> a -> OrderedMap k a -> OrderedMap k a
insert k a om@OrderedMap{..} =
  case (liftMapExtract_ (M.lookup k)) om of
    Nothing -> OrderedMap (M.insert k a map') (order ++ [k])
    -- If the key already exists, keep the old order
    _ -> OrderedMap (M.insert k a map') (order)

lookup :: Ord k => k -> OrderedMap k a -> Maybe a
lookup k = liftMapExtract_ (M.lookup k)

fromList :: Ord k => [(k, a)] -> OrderedMap k a
fromList kv = OrderedMap (M.fromList kv)  (map fst kv)

size :: OrderedMap k a -> Int
size = liftMapExtract_ M.size

keys :: OrderedMap k a -> [k]
keys (OrderedMap{order=order}) = order

elems :: Ord k => OrderedMap k a -> [a]
elems (OrderedMap{order=order, map'=map'}) = map (map' M.!) order

union :: (Eq k, Ord k) => OrderedMap k a -> OrderedMap k a -> OrderedMap k a
union (OrderedMap{order=o1, map'=m1}) (OrderedMap{order=o2, map'=m2}) =
  OrderedMap{map'=m1 `M.union` m2, order=L.nub(o1++o2)}

-- | Return the list of key value pairs in the order of insertion.
toList :: (Ord k, Pretty k, Pretty a) => OrderedMap k a -> [(k, a)]
toList omap = map (\k -> (k, omap OrderedMap.! k)) (keys omap)

adjust :: Ord k => (a -> a) -> k -> OrderedMap k a -> OrderedMap k a
adjust f k = liftMapEdit_ (M.adjust f k)

(!) :: (Ord k, Pretty k, Pretty a) => OrderedMap k a -> k -> a
ok ! k =
  case (OrderedMap.lookup k ok) of
           Just a -> a
           Nothing -> error . docToString $
               vcat [pretty "key missing, has no value associated with it: " <+> pretty k, indent 4 (pretty ok)]



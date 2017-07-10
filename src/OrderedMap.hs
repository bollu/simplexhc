{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}
module OrderedMap(OrderedMap,
  fromList,
  size,
  adjust,
  insert, 
  OrderedMap.lookup) where
import qualified Data.Map.Strict as M
import Data.Monoid

-- At some point, I need this. This is more convenient than overloading the key to store the insertion time.
-- | A dictionary that orders elements by insertion time
data OrderedMap k v = OrderedMap { map' :: M.Map k v, order :: [k] } deriving(Show, Functor, Foldable, Traversable)

instance Ord k => Monoid (OrderedMap k v) where
  mempty :: OrderedMap k v
  mempty = OrderedMap mempty mempty

  mappend :: OrderedMap k v -> OrderedMap k v -> OrderedMap k v
  mappend (OrderedMap m o) (OrderedMap m' o') = OrderedMap (m <> m') (o <> o')

liftMapEdit_ :: (M.Map k v -> M.Map k v) -> OrderedMap k v -> OrderedMap k v
liftMapEdit_ f (OrderedMap map' order) = OrderedMap (f map') order

liftMapExtract_ :: (M.Map k v -> a) -> OrderedMap k v -> a
liftMapExtract_ f (OrderedMap map' _) = f map'

insert  :: Ord k => k -> a -> OrderedMap k a -> OrderedMap k a
insert k a  = liftMapEdit_ (M.insert k a)

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


adjust :: Ord k => (a -> a) -> k -> OrderedMap k a -> OrderedMap k a
adjust f k = liftMapEdit_ (M.adjust f k)

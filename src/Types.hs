module Types
  ( emptyConstretto
  , lookup'
  , insert'
  ) where

import Data.Maybe
import qualified Data.Map.Strict as Map

data Constretto k v
  = Constretto (Map.Map k (Map.Map k v))
  | EmptyConstretto
  deriving (Eq, Show)

lookup' :: (Ord k) => k -> k -> Constretto k v -> Maybe v
lookup' _ _ EmptyConstretto = Nothing
lookup' tag key (Constretto mouter) = Map.lookup tag mouter >>= (Map.lookup key)

insert' :: (Ord k) => k -> k -> v -> Constretto k v -> Constretto k v
insert' tag key val EmptyConstretto =
  let inner = Map.insert key val Map.empty
      outer = Map.insert tag inner Map.empty
  in Constretto outer
insert' tag key val (Constretto outer) =
  let inner' = fromMaybe Map.empty (Map.lookup tag outer)
      inner = Map.insert key val inner'
      outer' = Map.insert tag inner outer
  in Constretto outer'

emptyConstretto :: Constretto k v
emptyConstretto = EmptyConstretto

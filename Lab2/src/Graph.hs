module Graph
  ( shortestPaths
  , Vertex
  , Weight
  , Graph
  , AdjMatrix
  ) where

import           Control.Monad.Par
import qualified Data.IntMap       as Map
import           Data.List

import           Util

-- Floyd–Warshall algorithm
shortestPaths :: [Vertex] -> Graph -> Graph
shortestPaths vs g = foldl' update g vs
  where
    update :: Graph -> Vertex -> Graph
    update g k =
      runPar $ do
        m <- Map.traverseWithKey (\i jmap -> spawn (return (shortmap i jmap))) g
        traverse get m
      where
        shortmap :: Vertex -> Map.IntMap Weight -> Map.IntMap Weight
        shortmap i jmap = foldr shortest Map.empty vs
          where
            shortest :: Vertex -> Map.IntMap Vertex -> Map.IntMap Weight
            shortest j m =
              case (old, new) of
                (Nothing, Nothing) -> m
                (Nothing, Just w)  -> Map.insert j w m
                (Just w, Nothing)  -> Map.insert j w m
                (Just w1, Just w2) -> Map.insert j (min w1 w2) m
              where
                old = Map.lookup j jmap
                new = do
                  w1 <- weight g i k
                  w2 <- weight g k j
                  return (w1 + w2)

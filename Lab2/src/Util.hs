module Util
  ( Vertex
  , Weight
  , Graph
  , AdjMatrix
  , weight
  , makeGraph
  ) where

import           Data.Array.Unboxed
import qualified Data.IntMap        as Map
import qualified Data.IntSet        as IntSet

type Vertex = Int

type Weight = Int

type Graph = Map.IntMap (Map.IntMap Weight)

type AdjMatrix = UArray (Int, Int) Weight

weight :: Graph -> Vertex -> Vertex -> Maybe Weight
weight g i j = do
  jmap <- Map.lookup i g
  Map.lookup j jmap

makeGraph :: [[Int]] -> Int -> Graph
makeGraph xss maxWeight = Map.fromList (zipWith row [0 ..] xss)
  where
    row i xs = (i, Map.fromList [(j, w) | (j, w) <- zip [0 ..] xs, w < maxWeight])

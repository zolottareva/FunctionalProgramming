import           Test.Tasty       (defaultMain, testGroup)
import           Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

import           Graph
import           Util

infinity :: Int
infinity = 10000

main :: IO ()
main = defaultMain tests

tests = testGroup "All tests" [shortestPathTest, shortestPathTestUnconnected]

shortestPathTest =
  testCase "Shortest Path Test for 3 vertices" $ do
    let adjMatrix = [[0, 1, infinity],
                     [1, 0, 1],
                     [infinity, 1, 0]]
    let graph = shortestPaths [0 .. 2] (makeGraph adjMatrix infinity)
    assertEqual "Shortest path 0 to 0 is not correct" (Just 0) (weight graph 0 0)
    assertEqual "Shortest path 0 to 1 is not correct" (Just 1) (weight graph 0 1)
    assertEqual "Shortest path 0 to 2 is not correct" (Just 2) (weight graph 0 2)
    assertEqual "Shortest path 1 to 2 is not correct" (Just 1) (weight graph 1 2)

shortestPathTestUnconnected =
  testCase "Shortest Path Test unconnected" $ do
    let adjMatrix = [[0, 1, infinity],
                     [1, 0, infinity],
                     [infinity, infinity, 0]]
    let graph = shortestPaths [0 .. 2] (makeGraph adjMatrix infinity)
    assertEqual "Shortest path 0 to 2 is not correct" Nothing (weight graph 0 2)
    assertEqual "Shortest path 1 to 2 is not correct" Nothing (weight graph 1 2)
    assertEqual "Shortest path 0 to 3 is not correct" Nothing (weight graph 0 3)
    assertEqual "Shortest path 1 to 3 is not correct" Nothing (weight graph 1 3)

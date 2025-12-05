module Main where

import AoCUtils
import Data.Set qualified as Set
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = case args of
        ("test" : _) -> "inputs/day04_test.txt"
        ("example" : _) -> "inputs/day04_example.txt"
        _ -> "inputs/day04.txt"

  putStrLn $ "Running with: " ++ inputFile
  grid <- readInputLines inputFile

  let part1 = solve1 grid
  putStrLn $ "Part 1: " ++ show part1

  let part2 = solve2 grid
  putStrLn $ "Part 2: " ++ show part2

solve1 :: [String] -> Int
solve1 input = length accessibleRolls
  where
    rolls = locateRolls input
    accessibleRolls = filter (< 4) $ map (`countNeighbors` rolls) (Set.toList rolls)

solve2 :: [String] -> Int
solve2 _ = 0

-- | locateRolls returns position-tuples, for every place a roll
-- | appears in the puzzle input (0 indexed positions)
-- | List-comp generates all (x,y) pairs that meet `isRollChar char` predicate
locateRolls :: [String] -> Set.Set (Int, Int)
locateRolls input =
  Set.fromList
    [ (x, y)
      | (y, row) <- zip [0 ..] input,
        (x, char) <- zip [0 ..] row,
        isRollChar char
    ]

-- | countNeighbors takes a a position, and the collection of rolls,
-- | and returns the number of neighboring rolls
countNeighbors :: (Int, Int) -> Set.Set (Int, Int) -> Int
countNeighbors (x, y) rolls =
  length
    [ (x', y')
      | x' <- [x - 1 .. x + 1],
        y' <- [y - 1 .. y + 1],
        (x', y') /= (x, y),
        Set.member (x', y') rolls
    ]

isRollChar :: Char -> Bool
isRollChar '@' = True
isRollChar _ = False

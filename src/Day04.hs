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

isRollChar :: Char -> Bool
isRollChar = (== '@') -- true if character is @

solve1 :: [String] -> Int
solve1 input = Set.size . removableRolls $ locateRolls input

-- | Similar to part1, but iteratively refine until we can't remove any more
-- | doStep computes set of removable rolls, then recurses with the updated total & rolls-state
solve2 :: [String] -> Int
solve2 input = doStep 0 (locateRolls input)
  where
    doStep totalRemoved rolls
      | Set.null (removableRolls rolls) = totalRemoved -- can't remove any more, return total
      | otherwise = doStep newTotal remainingRolls -- update and recurse
      where
        newTotal = totalRemoved + Set.size (removableRolls rolls)
        remainingRolls = Set.difference rolls (removableRolls rolls)

-- | locateRolls takes string-input lines, and returns position-tuples for every roll
-- | List-comp generates all (x,y) pairs that meet `isRollChar char` predicate
locateRolls :: [String] -> Set.Set (Int, Int)
locateRolls input =
  Set.fromList
    [ (x, y)
      | (y, row) <- zip [0 ..] input,
        (x, char) <- zip [0 ..] row,
        isRollChar char
    ]

-- | countNeighbors takes a pos and set of rolls, and returns the count
-- | List-comp generates adjacent positions, predicated on the position having a roll
countNeighbors :: (Int, Int) -> Set.Set (Int, Int) -> Int
countNeighbors (x, y) rolls =
  length
    [ (x', y')
      | x' <- [x - 1 .. x + 1],
        y' <- [y - 1 .. y + 1],
        (x', y') /= (x, y),
        Set.member (x', y') rolls -- in-case neighbor position is off the board e.g. (-1,-1)
    ]

-- | isRemovable checks if a roll has <4 neighbors
isRemovable :: Set.Set (Int, Int) -> (Int, Int) -> Bool
isRemovable rolls pos = countNeighbors pos rolls < 4

-- | removableRolls returns all rolls that can immediately be removed
-- | Set.filter checks each roll against the (isRemovable rolls) predicate
-- | (isRemovable rolls) is partial application to include the overall board state
removableRolls :: Set.Set (Int, Int) -> Set.Set (Int, Int)
removableRolls rolls = Set.filter (isRemovable rolls) rolls
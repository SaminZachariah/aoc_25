module Main where

import AoCUtils
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
solve1 input =
  let rolls = locateRolls input
      neighborCounts = map (`countNeighbors` rolls) rolls
      accessibleRolls = filter (< 4) neighborCounts
   in length accessibleRolls

solve2 :: [String] -> Int
solve2 _ = 0

-- | locateRolls returns a list of position-tuples, for every place a roll
-- | appears in the puzzle input
-- | [..., (x-pos, y-pos), ...] (0 indexed positions)
-- | 1st generator zips (y-idx-N, input-line-N) for each input line
-- | 2nd generator zips (x-pos-M, input-char-M) for each char in a line
-- | The 2nd generator gets the 'row' bound in the 1st generator
-- | and acts like a nested loop
-- | 'isRollChar char' is a boolean test to filter out non-roll characters
locateRolls :: [String] -> [(Int, Int)]
locateRolls input =
  [ (x, y)
    | (y, row) <- zip [0 ..] input,
      (x, char) <- zip [0 ..] row,
      isRollChar char
  ]

-- | countNeighbors takes a a position, and the list of rolls,
-- | and returns the number of neighboring rolls
countNeighbors :: (Int, Int) -> [(Int, Int)] -> Int
countNeighbors (x, y) rolls =
  let testPositions =
        [ (x', y')
          | x' <- [x - 1 .. x + 1],
            y' <- [y - 1 .. y + 1],
            (x', y') /= (x, y)
        ]
      neighborRolls = filter (`elem` rolls) testPositions
   in length neighborRolls

isRollChar :: Char -> Bool
isRollChar '@' = True
isRollChar _ = False

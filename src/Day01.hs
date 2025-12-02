module Main where

import AoCUtils
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = case args of
        ("test" : _) -> "inputs/day01_test.txt"
        ("example" : _) -> "inputs/day01_example.txt"
        _ -> "inputs/day01.txt"

  putStrLn $ "Running with: " ++ inputFile
  input <- readInputLines inputFile

  -- Part 1
  let part1 = solve1 input
  putStrLn $ "Part 1: " ++ show part1

  -- Part 2
  let part2 = solve2 input
  putStrLn $ "Part 2: " ++ show part2

solve1 :: [String] -> Int
solve1 input = zeroCount
  where
    rotations = map parseRotation input
    (_, zeroCount) = foldl step (50, 0) rotations

    step (pos, cnt) delta =
      let newPos = (pos + delta) `mod` 100
          newCnt = if newPos == 0 then cnt + 1 else cnt
       in (newPos, newCnt)

solve2 :: [String] -> Int
solve2 input = zeroCount
  where
    rotations = map parseRotation input
    (_, zeroCount) = foldl step (50, 0) rotations

    step (pos, cnt) delta =
      let newPos = (pos + delta) `mod` 100
          crossings =
            if delta >= 0
              then (pos + delta) `div` 100 - pos `div` 100
              else (pos - 1) `div` 100 - (pos + delta - 1) `div` 100
          newCnt = cnt + crossings
       in (newPos, newCnt)

-- | Parse rotation strings: L{n} -> -n, R{n} -> n
parseRotation :: String -> Int
parseRotation ('L' : n) = negate (read n)
parseRotation ('R' : n) = read n
parseRotation s = error $ "Invalid rotation: " ++ s

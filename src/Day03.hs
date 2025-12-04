module Main where

import AoCUtils
import Data.Char (digitToInt)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = case args of
        ("test" : _) -> "inputs/day03_test.txt"
        ("example" : _) -> "inputs/day03_example.txt"
        _ -> "inputs/day03.txt"

  putStrLn $ "Running with: " ++ inputFile
  rawInput <- readInput inputFile

  let banks = parseLines (map digitToInt) rawInput
  putStrLn $ "Banks: " ++ show banks

  -- Part 1
  let part1 = solve1 banks
  putStrLn $ "Part 1: " ++ show part1

  -- Part 2
  let part2 = solve2 banks
  putStrLn $ "Part 2: " ++ show part2

type Bank = [Int]

solve1 :: [Bank] -> Int
solve1 = sum . map bankJoltageP1

bankJoltageP1 :: Bank -> Int
bankJoltageP1 bank =
  case reverse bank of -- we want to traverse right-to-left
    (ones : tens : rest) ->
      -- one is before ten in reversed list
      pairJoltage $ foldl newBestPair (tens, ones) rest
      where
        pairJoltage (t, o) = t * 10 + o

        newBestPair (t, o) e -- Update curr-best with candidate element
          | e < t = (t, o) -- drop e (it's strictly worse)
          | t < o = (e, o) -- use e, drop tens (tens worse than ones)
          | otherwise = (e, t) -- use e, use tens
    _ -> error "Bank must have at least 2 elements"

solve2 :: [Bank] -> Int
solve2 = sum . map bankJoltageP2

bankJoltageP2 :: Bank -> Int
bankJoltageP2 bank =
  intFromDigits $ foldl updateCell initialCell rest
  where
    initialCell = reverse $ take 12 $ reverse bank
    rest = drop 12 $ reverse bank

-- recursively update curr-best choices with a candidate element
updateCell :: [Int] -> Int -> [Int]
updateCell [] _ = [] -- base case, nothing left to compare
updateCell (x : xs) e
  | e < x = x : xs -- drop e (it's worse)
  | otherwise = e : updateCell xs x -- use e, recurse to use/lose x on xs

-- helper to convert [1,2,3] => ["1", "2", "3"] => "123" => 123
intFromDigits :: [Int] -> Int
intFromDigits = read . concatMap show

module Main where

import AoCUtils
import Data.List.Split (chunksOf)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = case args of
        ("test" : _) -> "inputs/day02_test.txt"
        ("example" : _) -> "inputs/day02_example.txt"
        _ -> "inputs/day02.txt"

  putStrLn $ "Running with: " ++ inputFile
  input <- readInput inputFile

  putStrLn $ "Raw Input: " ++ show input

  -- Part 1
  let part1 = solve1 input
  putStrLn $ "Part 1: " ++ show part1

  -- Part 2
  let part2 = solve2 input
  putStrLn $ "Part 2: " ++ show part2

solve1 :: String -> Int
solve1 input = sum $ mapMaybe checkInvalidp1 listOfIds
  where
    listOfIds = concatMap generateRange (splitOn "," input)

solve2 :: String -> Int
solve2 input = sum $ mapMaybe checkInvalidp2 listOfIds
  where
    listOfIds = concatMap generateRange (splitOn "," input)

generateRange :: String -> [String]
generateRange productRange =
  case splitOn "-" productRange of
    [start, end] -> map show [read start .. read end :: Int]
    _ -> error "Invalid range format"

checkInvalidp1 :: String -> Maybe Int
checkInvalidp1 s
  | firstHalf == secondHalf = Just (read s :: Int)
  | otherwise = Nothing
  where
    (firstHalf, secondHalf) = splitAt (length s `div` 2) s

checkInvalidp2 :: String -> Maybe Int
checkInvalidp2 s
  | any isRepeatedPattern divisors = Just (read s :: Int)
  | otherwise = Nothing
  where
    divisors = [d | d <- [1 .. length s `div` 2], length s `mod` d == 0]

    isRepeatedPattern chunkSize =
      let chunks = chunksOf chunkSize s
       in all (== head chunks) (tail chunks) && length chunks >= 2
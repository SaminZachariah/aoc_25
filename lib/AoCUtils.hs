{-# LANGUAGE OverloadedStrings #-}

module AoCUtils
  ( -- * Input Reading
    readInput,
    readInputLines,
    readInputText,

    -- * Parsing Helpers
    parseLines,
    parseInts,
    parseGrid,
    splitOn,
    splitOnBlankLines,

    -- * Common Operations
    count,
    applyN,
  )
where

-- import Data.List (intercalate)

import Data.List.Split (splitOn)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

-- | Read input file as a String
readInput :: FilePath -> IO String
readInput = readFile

-- | Read input file as a list of lines
readInputLines :: FilePath -> IO [String]
readInputLines = fmap lines . readFile

-- | Read input file as Text
readInputText :: FilePath -> IO T.Text
readInputText = TIO.readFile

-- | Parse each line with a function
parseLines :: (String -> a) -> String -> [a]
parseLines f = map f . lines

-- | Parse integers from lines (one integer per line)
parseInts :: String -> [Int]
parseInts = map read . lines

-- | Parse a 2D grid into list of lists
parseGrid :: String -> [[Char]]
parseGrid = lines

-- | Split input on blank lines (useful for grouped data)
splitOnBlankLines :: String -> [String]
splitOnBlankLines = map unlines . splitOn [""] . lines

-- | Count elements satisfying a predicate
count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

-- | Apply a function n times
applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = applyN (n - 1) f (f x)

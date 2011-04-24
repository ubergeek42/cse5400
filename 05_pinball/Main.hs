{-
 - Author: Keith Johnson, kjohns07@my.fit.edu
 - Course: CSE5400, Spring 2011
 - Project: pachinko
-}
{-# OPTIONS_GHC -Wall -O2 #-}

module Main where

main :: IO()
main = interact (show . solve . readTestCases)

-- This builds a list of lists representing the problem
readTestCases :: String -> [[Int]]
readTestCases = parse 1 . map read . words
-- Input: Row#, list of ints from input
-- Output: returns a list of lists
parse :: Int->[Int]->[[Int]]
parse _ [] = []
parse 1 (_:x:xs) = [x]:parse 2 xs
parse i xs = (take i xs):parse (i+1) (drop i xs)

-- Solve the problem for one test case
solve :: [[Int]] -> Int
solve xs = head (getRowScores xs 0)

-- Arguments:
--    List of lists(all of the rows/values in the puzzle)
--    Current row index
-- Returns: score values for a given row
getRowScores:: [[Int]] -> Int -> [Int]
getRowScores xs row
    | row + 1 == length xs = xs !! row -- Bottom row, the values are simply what they are
    | otherwise            = cur       -- Otherwise, we have to compute the values based on the row below us
    where
        next = getRowScores xs (row + 1)               -- Get the scores for the row below us
        cur = map (getScore (xs !! row) next) [0..row] -- Update all the scores on our row

-- Given the current row, the row below us, and the position in the current row
-- Return the max value of this nail(current value + max of going either left or right)
getScore :: [Int]->[Int]->Int->Int
getScore cur next c = (cur!!c) + max left right
      where
        left = next !! c
        right = next !! (c+1)
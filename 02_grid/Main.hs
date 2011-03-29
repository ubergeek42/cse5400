{-
 - Author: Keith Johnson, kjohns07@my.fit.edu
 - Course: CSE5400, Spring 2011
 - Project: grid
-}

module Main where

import Text.Printf

main :: IO()
main = interact(display . map calculate . readInput)

type InstanceType = (Int, Int)
type ResultType = Int

-- Make a list of integers, and pass that to getPairs
readInput :: String -> [InstanceType]
readInput = getPairs . map read . words

-- Take a list of int's, and anytime there are 2 of them, prepend them to a list
-- If there is a zero, return an empty array; this stops the recursiveness of it
-- making interact terminate on eof?
getPairs :: [Int] -> [InstanceType]
getPairs (0:_) = []
getPairs (n:m:rest) = (n,m) : getPairs rest
getPairs _ = error "Invalid input"

-- Basic factorial function
fact :: Int -> Int
fact 0 = 1
fact x = x*fact (x-1)

-- Given a n x m grid, each path must be n+m in length, and each path must contain n Ups, and m Rights
-- This basically boils down to m+n choose m, or m+n choose n, which is effectively (n+m)!/(n!m!)
calculate :: InstanceType -> ResultType
calculate (a,b) = fact (a+b) `div` ((fact a) * (fact b))

-- Print the results
display :: [ResultType] -> String
display = unlines . (map format)
format n = printf "ans: %d" n
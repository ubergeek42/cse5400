{-
 SER 2008.  Problem B: Heart of the Country
 Author: Ryan Stansifer
-}
module Main where

import Data.List (foldl', (\\)) -- import additional list functions
import Text.Printf              -- import text formating function

{- unboxed arrays (Data.Array.Unboxed) do not result in measurable improvement. -}
import Data.Array

main :: IO()
main = interact (showResults . map total . readTestCases)

readTestCases :: String -> [(Int, Array Int Int, [[Int]])]
readTestCases = parse . (map read) . words

showResults :: [(Int,Int)] -> String
showResults = unlines . (map format)

-- break the input up into a list of individual test cases
parse :: [Int] -> [(Int, Array Int Int, [[Int]])]
parse (0:0:rest) = []
parse (n:k:rest) = parse1 (n,k) ([],[]) rest

parse1 (0,k) (a,b) rest       = (k, q a, z b) : (parse rest)
parse1 (n,k) (a,b) (t:m:rest) = parse1 (n-1,k) (t:a,(take m rest):b) (drop m rest)

-- Use an immutable array for the number of troops stationed at a city.
q as = listArray (0, length as) (reverse as) :: Array Int Int
-- It is convenient to add the index of the city at the head of its adjacency list
z adj = zipWith (:) [0..] (reverse adj)

total :: (Int, Array Int Int, [[Int]]) -> (Int, Int)
total (k,r,adj) = g (k,r,adj) js
   where
      -- The indices of non-heart cities who are now no longer defensible
      js =  [head is| is<-adj, not(null is), (defenders r is)<k] :: [Int]

defenders :: Array Int Int -> [Int] -> Int
defenders r = foldl' f 0 where f x y = x+r!y

-- Recurse if necessary after eliminating indefensible cities, otherwise return total
g (k,r,adj) [] = (length js, defenders r js) where js=[head is |is<-adj, not(null is)]
g (k,r,adj) is = total (k,r,map (eliminate is) adj)

eliminate :: [Int] -> [Int] -> [Int]
eliminate is [] = []
eliminate is (h:tl) = if h `elem` is then [] else h:(tl \\ is)

format :: (Int,Int) -> String
format (n,t) = printf "%d %d" n t

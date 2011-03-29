{-
 SER 2008.  Problem C: Lawrence of Arabia
 Author: Ryan Stansifer
-}
{-# OPTIONS_GHC -O2       #-}

module Main where

import Data.List
import Text.Printf
import Data.Array

main :: IO()
main = interact (showResults . map minStrategicValue . readTestCases)

readTestCases :: String -> [(Int, Int, Array Int Int)]
readTestCases = parse . (map read) . words

showResults :: [Int] -> String
showResults = unlines . (map format)

-- break the input up into a list of individual test cases
parse :: [Int] -> [(Int, Int, Array Int Int)]
parse (0:0:rest) = []
parse (n:m:rest) = (n, m, listArray (0,n-1) (take n rest)) : parse (drop n rest)

format :: Int -> String
format n = printf "%d" n

minStrategicValue :: (Int,Int,Array Int Int) -> Int
minStrategicValue (n,m,depot) = msva!(m,n-1)
   where
      -- partial sums
      suma = array (0,n) [(i,f i)|i<-[0..n]]
      f i = if i==0 then 0 else suma!(i-1)+(depot!(i-1))

      -- strategic value
      valuea = array ((0,0),(n-1,n-1)) [((i,j),g i j)|i<-[0..n-1],j<-[i..n-1]]
      g i j = if i==j then 0 else (valuea!(i,j-1)) + (suma!j - suma!i) * (depot!j)

      msva = array ((0,0),(m,n-1)) [((k,p),h k p)|k<-[0..m],p<-[0..n-1]] --p<=k
      h k p = if k==0 then valuea!(0,p) else minimum [msva!(k-1,x) + valuea!(x+1,p)| x<-[k-1..p-1]]

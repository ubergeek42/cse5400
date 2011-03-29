{-
 SER 2010.  Problem A: Balloons
 Author: Ryan Stansifer
-}
module Main where

import Debug.Trace
import Text.Printf              -- import text formating function
import Data.List (sortBy)

main :: IO()
main = interact (showResults . map total . readTestCases)

type InstanceType = (Int,Int,[(Int,Int,Int)])
type ResultType = Int

readTestCases :: String -> [InstanceType]
readTestCases = parse0 . (map read) . words

showResults :: [ResultType] -> String
showResults = unlines . (map format)
format n = printf "%d" n

-- break the input up into a list of individual test cases
parse0 :: [Int] -> [InstanceType]
parse0 (0:rest) = []
parse0 (n:a:b:rest)  = parse1 (n,a,b) rest

parse1 :: (Int,Int,Int)-> [Int] -> [InstanceType]
parse1 (n,a,b) rest = (a,b,(triples (take (3*n) rest) [])) : (parse0 (drop (3*n) rest))

triples :: [Int] -> [(Int,Int,Int)] -> [(Int,Int,Int)]
triples [] acc = acc
triples (x:y:z:rest) acc = triples rest ((x,y,z):acc)

total :: InstanceType -> ResultType
total (a,b,ts) = let (_,_,d) = foldr totalTeam (a,b,0) (sortBy absDiff ts) in d

absDiff (_,d1,d2) (_,d1',d2') = compare (abs (d1-d2)) (abs (d1'-d2'))


preferA (_,d1,d2) = d1<d2
--preferB (_,d1,d2) = d1>d2
preferB (_,d1,d2) = not (d1<d2)
enoughA (n,d1,d2) (a,b,d) = n<=a
enoughB (n,d1,d2) (a,b,d) = n<=b

totalTeam t@(n,d1,d2) state@(a,b,d)
  | n > a+b = error "not enough"
--  | (preferA t) && (enoughA t state) = trace (show (n,a-n,b,d+n*d1)) (a-n,b,d+n*d1)
  | (preferA t) && (enoughA t state) = (a-n,b,d+n*d1)
  | (preferB t) && (enoughB t state) = (a,b-n,d+n*d2)
  | preferA t = let r=n-a in (0,b-r,d+a*d1+r*d2)
  | preferB t = let r=n-b in (a-r,0,d+r*d1+b*d2)
  | otherwise = error "impossible"

total1 (n,d1,d2) (a,b,d)  = (a,b,d)
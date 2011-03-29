
{-
 - Author: Keith Johnson, kjohns07@my.fit.edu
 - Course: CSE5400, Spring 2011
 - Project: game
-}

module Main where

import Text.Printf
import Data.List

main :: IO()
main = interact(display . map calculate . readInput)

type Triangle = (Int,Int,Int)
type Hexagon = [Triangle]
type ResultType = Int


-- Make a list of integers, and pass that to getPairs
readInput :: String -> [Hexagon]
readInput = getProblems . words

   -- Reads in a list of problems, and stops when it hits a $
getProblems :: [String] -> [Hexagon]
getProblems ("$":_) = []
getProblems ("*":rest) = getProblems rest
getProblems x = (getTriangles (take (3*6) x)):(getProblems (drop (3*6) x))

-- Takes a list of strings, and turns it into a list of triples
getTriangles :: [String] -> Hexagon
getTriangles [] = []
getTriangles (i:j:k:rest) = (read i, read j, read k) : getTriangles rest
getTriangles _ = error "Invalid input"

-- Print the results
display :: [ResultType] -> String
display = unlines . (map format)
format 0 = printf "none"
format n = printf "%d" n

-----------------------------------------------
-- All calculation related things below here --
-----------------------------------------------
-- Rotates a Triangle(left or right)
rotTripleL :: Triangle -> Triangle
rotTripleL (a,b,c) = (b,c,a)
rotTripleR :: Triangle -> Triangle
rotTripleR (a,b,c) = (c,a,b)

-- Permute the triangles in an hexagon according to a list
-- The list contains 6 elements, which determine if the tile should be rotated left, right, or not at all
permuteTriangles :: ([Int],Hexagon)->Hexagon
permuteTriangles (p,t) = doPermutation (zip p t)
doPermutation :: [(Int, Triangle)] -> Hexagon
doPermutation [] = []
doPermutation (x:rest)
    | permutation==1   = (rotTripleL triple):doPermutation rest
    | permutation==2   = (rotTripleR triple):doPermutation rest
    | otherwise        = triple:doPermutation rest
    where (permutation,triple) = x


-- Checks if a list of triangles is a valid hexagon arrangement
validArrangement :: Hexagon->Bool
validArrangement x = and [x==y | ((_,x,_),(y,_,_))<-zip lista listb]
    where lista = x
	  listb = (tail x) ++ ((head x):[])

-- Finds the value of the outside edges of a given hexagon of triangles
findValue :: Hexagon->Int
findValue x
    | (validArrangement x) == True  = getValue x
    | otherwise                     = 0
getValue :: Hexagon->Int
getValue [] = 0
getValue ((_,_,x):rest) = x + (getValue rest)

{--
  - Does all of the magic
  - Permutes all positions of triangles
  - Creates a list containing all possible hexagon arrangements after rotating tiles
  - Finds the value of each hexagon
  - Finds the maximum value of those results
  -- Fairly certain this is inefficient/suboptimal(I believe the list length is 524880)
--}
calculate :: Hexagon -> ResultType
calculate x = foldl1' max (map findValue . map permuteTriangles . collapseList $ map buildPermutations $ permutations x)

-- Takes a list of lists, and returns a single list, with all sublists being added to the main list
collapseList :: [[([Int],Hexagon)]]->[([Int],Hexagon)]
collapseList x = [i | z<-x, i<-z]

-- Creates a list of permutations to apply to a hexagon(in the form of rotations to apply per tile)
buildPermutations :: Hexagon->[([Int],Hexagon)]
buildPermutations x = zip [ a:b:c:d:e:f:[] | a<-[0,1,2], b<-[0,1,2], c<-[0,1,2], d<-[0,1,2], e<-[0,1,2], f<-[0,1,2]] (repeat x)
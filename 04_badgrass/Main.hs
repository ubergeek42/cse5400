{-
 - Author: Keith Johnson, kjohns07@my.fit.edu
 - Course: CSE5400, Spring 2011
 - Project: grass
-}

module Main where

import Text.Printf
import Data.Array
import Debug.Trace

main :: IO()
main = interact(display . map calculate . readInput)

-- Rows, Columns, Array representing the "map"
type GrassMap = (Int, Int, Array (Int,Int) Int)

-- Make a list of integers, and pass that to getPairs
readInput :: String -> [GrassMap]
readInput = parseNumbers . map read . words

-- (Rows, Columns) [Entire input]
parseNumbers :: [Int] -> [GrassMap]
parseNumbers [] = []
parseNumbers (h:w:xs) = (w,h,gmap) : parseNumbers rest
    where
      (gmap,rest) = getArray w h xs

-- Builds an array from the input
getArray:: Int->Int->[Int]->(Array (Int,Int) Int, [Int])
--getArray _ _ [] = (array (0,0), [])
getArray 0 _ _ = error "Invalid width(Must be greater than zero)"
getArray _ 0 _ = error "Invalid height(Must be greater than zero)"
getArray w h xs = (array ((0,0),(h-1,w-1)) (zip ix mdata) , rest)
    where
          ix = range ((0,0),(h-1,w-1))
          mdata = take (w*h) xs
          rest = drop (w*h) xs

-- Given a n x m grid, each path must be n+m in length, and each path must contain n Ups, and m Rights
-- This basically boils down to m+n choose m, or m+n choose n, which is effectively (n+m)!/(n!m!)
calculate :: GrassMap -> Int
calculate (w,h,gmap) = countIslands (findNonZero gmap) gmap

countIslands :: [(Int,Int)]->Array (Int,Int) Int -> Int
countIslands [] _ = 0
countIslands ((x,y):_) gmap = n + (countIslands (findNonZero newmap) newmap)
    where
        (newmap,n) = clearIsland x y gmap

-- Clears out an "island" starting at x,y
-- Returns 1 if cleared an island, otherwise 0
clearIsland:: Int->Int->Array (Int,Int) Int -> (Array (Int,Int) Int, Int)
clearIsland x y gmap | (x,y) `notElem` (indices gmap) = (gmap,0)
                     | gmap!(x,y)==0 = (gmap, 0)
                     | otherwise     = (ret,  1)
    where
        ret = fst $ clearIsland   (x)   (y+1) $
                fst $ clearIsland (x)   (y-1) $
                fst $ clearIsland (x+1) (y+1) $
                fst $ clearIsland (x+1) (y)   $
                fst $ clearIsland (x+1) (y-1) $
                fst $ clearIsland (x-1) (y+1) $
                fst $ clearIsland (x-1) (y)   $
                fst $ clearIsland (x-1) (y-1) (gmap // (((x,y),0):[]))


findNonZero :: Array (Int,Int) Int -> [(Int,Int)]
findNonZero gmap =  x --trace (show x)
  where
    x = [ i | i<-(indices gmap), gmap!i /= 0]

-- -- printArray:: Array (Int,Int) Int -> String
-- printArray arr =

-- Print the results
display :: [Int] -> String
display = unlines . (map format)
format n = printf "%d" n
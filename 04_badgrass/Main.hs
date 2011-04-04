{-
 - Author: Keith Johnson, kjohns07@my.fit.edu
 - Course: CSE5400, Spring 2011
 - Project: grass
-}

module Main where

import Text.Printf
import Data.Array.Diff

main :: IO()
main = interact(display . map calculate . readInput)


-- A 2D array that holds Ints
type GrassMap = DiffUArray (Int,Int) Int
-- Width, Height, Array representing the grass
type Pasture = (Int, Int, GrassMap)

-- Make a list of integers, and pass that to parseNumbers
readInput :: String -> [Pasture]
readInput = parseNumbers . map read . words

{- Takes 2 numbers from the front, calls them the width/height, and creates an
 - array from the input of that size.  It then recursively calls itself until
 - there is no more input left.
 -}
parseNumbers :: [Int] -> [Pasture]
parseNumbers [] = []
parseNumbers (h:w:xs) = (w,h,gmap) : parseNumbers rest
    where
      (gmap,rest) = getArray w h xs

{- Takes in the width, height, and the rest of the input
 - Returns a pair containing:
   * a new 2D Data.Array of the specified width/height
   * Any input that was unused
 -}
getArray:: Int->Int->[Int]->(GrassMap, [Int])
getArray 0 _ _ = error "Invalid width(Must be greater than zero)"
getArray _ 0 _ = error "Invalid height(Must be greater than zero)"
getArray w h xs = (array ((0,0),(h-1,w-1)) (zip ix mdata) , rest)
    where
          ix = range ((0,0),(h-1,w-1))
          mdata = take (w*h) xs
          rest = drop (w*h) xs

-- Takes a "Pasture" and returns the number of islands in it
calculate :: Pasture -> Int
calculate (w,h,gmap) = countIslands (findNonZero gmap) gmap

{- Provided a list of all locations that are non-zero(i.e. part of an island)
 - and a 2d array of the pasture, this function does the following:
   * takes the first non zero location, and performs a flood clear on it
   * calls itself recursively with the updated map from the clear, and an
     updated list of non-zero locations
 - This method terminates when there are no more non-zero locations(or islands)
 - left to check.
 -}
countIslands :: [(Int,Int)]->GrassMap-> Int
countIslands [] _ = 0
countIslands ((x,y):_) gmap = 1 + (countIslands (findNonZero newmap) newmap)
    where
        newmap = clearIsland x y gmap

--doClear :: GrassMap->[(Int,Int)]->(GrassMap, Int)
--doClear gmap [] = (gmap, 0)
--doClear gmap toclear = (gmap // (zip toclear (repeat 0)),1)

{- Does a flood-clear starting at location x,y
 - Returns 0 if there was nothing to clear(i.e. no island here)
 - Returns 1 if it cleared something(i.e. there was an island here)
 -
 - Does the naive thing and clears the current element, then recursively calls
 - itself on each neighboring element, passing the intermediate results along
 -}
clearIsland:: Int->Int->GrassMap->GrassMap
clearIsland x y gmap | (x<0) || (y<0) || (x>maxX) || (y>maxY) = gmap  -- Skip out of bounds elements
                     | gmap!(x,y) == 0                = gmap  -- Skip places that are zero(bounds of the island)
                     | otherwise                      = ret   -- Recurse to all surrounding blocks
    where
        ((_,_),(maxX,maxY)) = bounds gmap
        -- The use of fst through here is just to get rid of the number, which
        -- only matters to the calling function(and not to the recursive calls)
        ret = clearIsland   (x)   (y+1) $
                clearIsland (x)   (y-1) $
                clearIsland (x+1) (y+1) $
                clearIsland (x+1) (y)   $
                clearIsland (x+1) (y-1) $
                clearIsland (x-1) (y+1) $
                clearIsland (x-1) (y)   $
                clearIsland (x-1) (y-1) gmapn
            where
                gmapn = gmap // (((x,y),0):[])

{- This takes an array and returns a list of coordinates that contain non-zero
 - values in them.  The list comprehension iterates over the entire list of
 - possible indices, and comparing the value at those locations to zero.
 -}
findNonZero :: GrassMap-> [(Int,Int)]
findNonZero gmap =  x
  where
    x = [ i | i<-(indices gmap), gmap!i /= 0]

-- Print the results
display :: [Int] -> String
display = unlines . (map format)
format n = printf "%d" n

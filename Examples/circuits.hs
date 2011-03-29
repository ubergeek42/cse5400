{-
 SER 2008.  Problem A: Series/Parallel Resistor Circuits
 Author: Ryan Stansifer
-}
module Main where

import Text.Printf              -- import text formating function

main :: IO()
main = interact (showResults . map resistance . readTestCases)

type InstanceType = [(Char,Char,Float)]
type ResultType = Float

readTestCases :: String -> [InstanceType]
readTestCases = parse0 . words

showResults :: [ResultType] -> String
showResults = unlines . (map format)

-- break the input up into a list of individual test cases
parse0 :: [String] -> [InstanceType]
parse0 (x:rest)  = parse1 (read x) rest
parse1 :: Int -> [String] -> [InstanceType]
parse1 0 _ = []
parse1 n rest = (triples (take (3*n) rest) []) : (parse0 (drop (3*n) rest))

triples :: [String] -> InstanceType -> InstanceType
triples [] acc = acc
triples (x:y:z:rest) acc = triples rest (add (head x, head y, read z) acc)

source = 'A'
sink   = 'Z'
endp a = (a==source || a==sink)
endt a (x,y,_) = (a==x || a==y)

add  :: (Char,Char,Float) -> InstanceType -> InstanceType
add t [] = [t]
add t (x:xs) = if equ t x then (par t x):xs else x : (add t xs)
   where
      equ (a,b,_) (x,y,_) = (a==x && b==y) || (a==y && b==x)
      par (a,b,f) (x,y,g) = (a, b, inv f g);
      inv f g = 1.0/(1.0/f + 1.0/g)


degree :: InstanceType -> Char -> Int
degree ts a = length (filter (endt a) ts)

candidates :: InstanceType -> [Char]
candidates ts = [x|x<-['B'..'Y'], degree ts x == 2]

resistance = analyze . resistance2

analyze :: InstanceType -> ResultType
analyze [('A','Z',f)] = f
analyze [('Z','A',f)] = f
analyze _ = -1.0

resistance2 :: InstanceType -> InstanceType
resistance2 l = (recurse (candidates l) l)

recurse :: [Char] -> InstanceType -> InstanceType
recurse []    l = l
recurse (m:_) l = resistance2 (update m l)

merge m (a,b,f) (x,y,g) 
  |m==a && m==x = (b,y,f+g)
  |m==a && m==y = (b,x,f+g)
  |m==b && m==x = (a,y,f+g)
  |m==b && m==y = (a,x,f+g)
  |otherwise = error "merge"

update :: Char -> InstanceType -> InstanceType
update m ts = ts'
  where [t,t'] = filter (endt m) ts
        ys = filter (not . (endt m)) ts
        -- ([t,t'],ys) = List.partition (endt m) ts
        ts' = add (merge m t t') ys
        
format n = printf "%6.3f" n

import Data.List
import Data.List.Split
import Data.Char
import Control.Monad (join)
import Control.Monad (msum)
import Data.Monoid (mconcat)

sum' [] = 0
sum' (x:xs) = x + sum'(xs)

append' [] = []
append' (x:xs) = [x] ++ append'(xs)

reverse' [] = []
reverse' (x:xs) = reverse'(xs) ++ [x]

insertSorted [] y = [y]
insertSorted (x:xs) y | y<x = y:x:xs
					  | y>x = x:insertSorted(xs) y

isort [] = []
isort (x:xs) = insertSorted(isort xs) x

elem' [] y = False
elem' (x:xs) y | x==y = True
			  | otherwise = elem'(xs) y

helpmin [] = 0
helpmin (x:xs) = x

min' [] = 0
min' x = helpmin (isort x)

zip' [][] = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip'(xs)(ys)

-- split []= ([],[])
-- split [x]= ([x],[])
-- split(x1:x2:xs)= (x1:fst(split(xs)),x2:snd(split(xs)))

replicate' x [] = [x]
replicate' x (0:ys) = [x] ++ replicate' x (ys)
-- len [] = 0
-- len (x:xs) = 1 + len(xs)

-- len [[]] = 0
-- len [x:xs] = 1 + len(xs(1+len(xs)))

--splitLine :: [String] -> Int -> ([String], [String])
-- Function definition here
splitLline [] _ = ([],[])
splitLine (xs) 0 = ([], (xs)) 
splitLine (x:xs) y | y - length(x) >= 0 = (x:fst(splitLine xs (y - 1 - length(x))) , snd(splitLine xs (y - 1 - length(x))))
				   | otherwise = (fst(splitLine xs 0) , x:snd(splitLine xs 0))


mapSquare a = map (^2) a

-------------------------- ------------------- --------------------------------


-- end

-- addSpaces [] y = []
-- addSpaces (x:xs) y | y > 0 = [x] ++ [" "] ++ map ((addSpaces(xs) 2) (x:xs))

-- bI y [] = ([])
-- bI y (x:xs) = map 

-- addSpaces [] y = []
-- addSpaces (x:xs) y | y > 0 = [x] ++ [" "] ++ map ((addSpaces(xs) 2) (x:xs))


-- function [] = []
-- function (x:xs) = 

--x ++ concatFunc(xs) (y-length(x))
-- x ++ concatFunc(xs) y

-- concat(x:fst(concatFunc (xs) (y - length(x)))) , snd(concatFunc xs (y - length(x))))
-- ((concatFunc xs 0) , (x:snd(concatFunc xs 0)))

newTest [] = []
newTest x = concat(splitChar(x))

hyphenate [] = []
hyphenate (x:xs) = [length(x)] ++ hyphenate(xs)

-- lineBreaks :: [(String, [String])] -> Int -> [String] -> [([String], [String])]
-- task2
concating [] = []
concating (x:xs) = x ++ concating(xs)

splitChar [] = []
splitChar (x:xs) = chunksOf 2 (x) ++ splitChar(xs)

-- concatFunc [] y = ([],[])
-- concatFunc (xs) 0 = ((xs) , [])
-- concatFunc (x:xs) y | y - length(x) > 0 = (fst([x] ++ concatFunc(xs) (y-length(x))) ,[ ([x] ++ concatFunc(xs) (y-length(x)))])
-- 					| otherwise = ([x] ++ concatFunc(xs) (y-length(x)) , snd([x] ++ concatFunc(xs) y))

-- compute [] y = ([],[])
-- compute (x:xs) y | y - length(x) >=0 = [x] ++ [compute(xs) (y - 1 - length(x))]
-- 				 | otherwise = concat(splitAt y (splitChar(x)))

rmSame [] = []
rmSame (x:xs) = x : rmSame (filter (/= x) xs)

addSpaces [] = []
addSpaces (x:xs) | x /= last(x:xs), x/= " " = (([x] ++ [" "]) ++ xs) : (map (x:) (addSpaces(xs)))
                 | otherwise = map (x:) (addSpaces(xs))

map' [] = []
map' x = addSpaces(x)

--test = ["A","creative","man"]
test' [] = []
test' x = map' x
test'' x y | y == 1 = map' x
test'' x y | y > 1 = rmSame(concat (map addSpaces (test'' x (y-1))))

blankInsertions y [] = [[]]
blankInsertions y x | y == 1 = test'(x)
					| y > 1 , y /= 0 = test'' x y

-- lineSpaces [] = []
-- lineSpaces (x:xs) = x ++ " " ++ xs ++ map x ++ lineSpaces(xs)







import Data.List
import Data.List.Split
import Data.Char
import Control.Monad (join)
import Control.Monad (msum)
import Data.Monoid (mconcat)

-- lineCost ["He", " ", " ", "who", "controls"] 
-- [a,b,c]
-- This gives a
-- count1 :: [String] -> Int -> Int
count1 [] y = y
count1 (x:xs) y | x /= " " = count1(xs) (y+1)
				| otherwise = count1(xs) y

-- This gives b
count2 :: [String] -> Int -> Int
count2 [] y = y
count2 (x:xs) y | x == " " = count2(xs) (y+1)
				| x /= " " = count2(xs) y

-- This gives c
-- count3 :: [String] -> Int -> Int
-- count3 [] y = y
-- count3 (x:xs) y | x == " " = count3(x:xs) 0
-- 				| x == " " , count3(xs) y /= " " = count3(xs) 1
-- 				| y >= 1 = count3(xs) (y+1)
length' [] = 0
length' (x:xs) = 1 + length'(xs)

count [] y = [y]
count (x:xs) y | x /= " " =  (count(xs)(y+1))
			   | x == " " = y : (count xs 0)
average' [] = 0.0
average' x = ((sum(x)) /length'(x))

firstLast [] = []
firstLast [x] = []
firstLast xs = tail (init xs)

-- variance :: [Int] -> Double
variance [] = 0.0
variance x = (sum((map (^2) (x)))/length'(x)) - (sum(x)/length'(x))^2

blankCost = 1.0
blankProxCost = 1.0
blankUnevenCost = 1.0
hypCost = 1.0

-- finalAnswer = blankCost * 2.0 + blankProxCost * (5 - average'count + blankUnevenCost * variance(1,0,2) + hypCost * 0.0


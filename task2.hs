import Data.List
import Data.List.Split
import Data.Char
import Control.Monad (join)
import Control.Monad (msum)
import Data.Monoid (mconcat)
variable = ["He", "who", "controls" , "the" , "world"]

enHyp = [("creative", ["cr","ea","ti","ve"]), ("controls", ["co","nt","ro","ls"]), ("achieve", ["ach","ie","ve"]), ("future", ["fu","tu","re"]), ("present", ["pre","se","nt"]), ("motivated", ["mot","iv","at","ed"]), ("desire", ["de","si","re"]), ("others", ["ot","he","rs"])]

splitLline [] _ = ([],[])
splitLine (xs) 0 = ([], (xs)) 
splitLine (x:xs) y | y - length(x) >= 0 = (x:fst(splitLine xs (y - 1 - length(x))) , snd(splitLine xs (y - 1 - length(x))))
				   | otherwise = (fst(splitLine xs 0) , x:snd(splitLine xs 0))

splitLline1 [] _ = ([],[])
splitLine1 (xs) 0 = ([], (xs)) 
splitLine1 (x:xs) y | y - length(x) >= 0 = (x:fst(splitLine1 xs (y - length(x))) , snd(splitLine1 xs (y - length(x))))
				   | otherwise = (fst(splitLine1 xs 0) , x:snd(splitLine1 xs 0))

-- lineBreaks :: [(String, [String])] -> Int -> [String] -> [([String], [String])]

removePunctation xs = [ x | x <- xs, not (x `elem` ".,?!:")]

secondArg [] _ = []
secondArg x y = concat(map snd [splitLine x y])!!0

secSec [] y = []
secSec (x:xs) y | removePunctation(y) == fst(x) = snd(x)
				| otherwise = secSec(xs) y

rmSame [] = []
rmSame (x:xs) = x : rmSame (filter (/= x) xs)
-- traverse' [] y = ([],[])
-- traverse' (xs) 0 = ([], (xs)) 
-- traverse' (x:xs) y | y - length(x) > 0 =  (x:fst(traverse' xs y), snd(traverse' xs y))
-- 				   | otherwise = (fst(traverse' xs 0) , concat((x:snd(traverse' xs 0))))

fullstopCheck:: String -> Char


iterator x1 x q [] = []
iterator x1 x q (y:ys) = [(x++[fst(y)!!0] , [snd(y)!!0]++tail(snd(splitLine x1 q)))]++ iterator x1 x q (ys)


fullstopCheck (x:xs) | last(x:xs) == '.' = '.'
fullstopCheck (x:xs) | otherwise = ' '

				  
concat' [] = []
concat' (x:xs) = x ++ concat'(xs)

firstConcat [] y w = ([],[])
firstConcat (x:xs) y w = ([concat'(fst(splitLine1 (x:xs) y)) ++ "-"],[concat'(snd(splitLine1 (x:xs) y)) ++ [fullstopCheck(w)]]) 


totalTuple [] y q w = []
totalTuple x y q w | y < q-1 = rmSame[firstConcat(x) y w] ++ rmSame(totalTuple x (y+1) q w) 
				   | otherwise = rmSame[]

-- lineBreaks enHyp 12 ["He", "who", "controls."]
-- lineBreaks :: [(String, [String])] -> Int -> [String] -> [([String], [String])]
--lineBreaks enHyp y x = [([], [])]
removing [] = []
removing x | x == [] = []
		   | otherwise = tail(x)

lineBreaks enHyp y x = [splitLine(x) y] ++ removing(iterator x (fst(splitLine(x) y)) y (totalTuple (secSec enHyp (secondArg x y))  1 ((y) - length(unwords(fst(splitLine(x) y)))) (snd(splitLine(x) y)!!0) ))





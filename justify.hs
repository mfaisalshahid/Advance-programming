-- ========================================================================================================================== --


--
--                                                          ASSIGNMENT 1
--
--      A common type of text alignment in print media is "justification", where the spaces between words, are stretched or
--      compressed to align both the left and right ends of each line of text. In this problem we'll be implementing a text
--      justification function for a monospaced terminal output (i.e. fixed width font where every letter has the same width).
--
--      Alignment is achieved by inserting blanks and hyphenating the words. For example, given a text:
--
--              "He who controls the past controls the future. He who controls the present controls the past."
--
--      we want to be able to align it like this (to a width of say, 15 columns):
--
--              He who controls
--              the  past cont-
--              rols  the futu-
--              re. He  who co-
--              ntrols the pre-
--              sent   controls
--              the past.
--


-- ========================================================================================================================== --


import Data.List
import Data.Char

text1 = "He who controls the past controls the future. He who controls the present controls the past."
text2 = "A creative man is motivated by the desire to achieve, not by the desire to beat others."


-- ========================================================================================================================== --


variable = ["He", "who", "controls" , "the" , "world"]





-- ========================================================= PART 1 ========================================================= --


--
-- Define a function that splits a list of words into two lists, such that the first list does not exceed a given line width.
-- The function should take an integer and a list of words as input, and return a pair of lists.
-- Make sure that spaces between words are counted in the line width.
--
-- Example:
--    splitLine ["A", "creative", "man"] 12   ==>   (["A", "creative"], ["man"])
--    splitLine ["A", "creative", "man"] 11   ==>   (["A", "creative"], ["man"])
--    splitLine ["A", "creative", "man"] 10   ==>   (["A", "creative"], ["man"])
--    splitLine ["A", "creative", "man"] 9    ==>   (["A"], ["creative", "man"])
--


splitLine :: [String] -> Int -> ([String], [String])
-- Function definition here
-- This will take a variable y that changes(decrements) as more and more words are added 
-- if it exceeds the length then it returns it into the second list
splitLline [] _ = ([],[])
splitLine (xs) 0 = ([], (xs)) 
splitLine (x:xs) y | y - length(x) >= 0 = (x:fst(splitLine xs (y - 1 - length(x))) , snd(splitLine xs (y - 1 - length(x))))
				   | otherwise = (fst(splitLine xs 0) , x:snd(splitLine xs 0))


-- ========================================================= PART 2 ========================================================= --


--
-- To be able to align the lines nicely. we have to be able to hyphenate long words. Although there are rules for hyphenation
-- for each language, we will take a simpler approach here and assume that there is a list of words and their proper hyphenation.
-- For example:

enHyp = [("creative", ["cr","ea","ti","ve"]), ("controls", ["co","nt","ro","ls"]), ("achieve", ["ach","ie","ve"]), ("future", ["fu","tu","re"]), ("present", ["pre","se","nt"]), ("motivated", ["mot","iv","at","ed"]), ("desire", ["de","si","re"]), ("others", ["ot","he","rs"])]


--
-- Define a function that splits a list of words into two lists in different ways. The first list should not exceed a given
-- line width, and may include a hyphenated part of a word at the end. You can use the splitLine function and then attempt
-- to breakup the next word with a given list of hyphenation rules. Include a breakup option in the output only if the line
-- width constraint is satisfied.
-- The function should take a hyphenation map, an integer line width and a list of words as input. Return pairs of lists as
-- in part 1.
--
-- Example:
--    lineBreaks enHyp 12 ["He", "who", "controls."]   ==>   [(["He","who"], ["controls."]), (["He","who","co-"], ["ntrols."]), (["He","who","cont-"], ["rols."])]
--
-- Make sure that words from the list are hyphenated even when they have a trailing punctuation (e.g. "controls.")
--
-- You might find 'map', 'find', 'isAlpha' and 'filter' useful.
--


lineBreaks :: [(String, [String])] -> Int -> [String] -> [([String], [String])]
-- Function definition here

--Same as the previous splitLine but this doesnt take space into count (I am using "unwords" inbuilt function to take that into account)
splitLline1 [] _ = ([],[])
splitLine1 (xs) 0 = ([], (xs)) 
splitLine1 (x:xs) y | y - length(x) >= 0 = (x:fst(splitLine1 xs (y - length(x))) , snd(splitLine1 xs (y - length(x))))
				   | otherwise = (fst(splitLine1 xs 0) , x:snd(splitLine1 xs 0))

-- removes punctuation so that we can search the word in enhyp
removePunctation xs = [ x | x <- xs, not (x `elem` ".,?!:")]

-- gives second arg of splitLine (to search in lists)
secondArg [] _ = []
secondArg x y = concat(map snd [splitLine x y])!!0

--gives hyphnated list of second arg
secSec [] y = []
secSec (x:xs) y | removePunctation(y) == fst(x) = snd(x)
				| otherwise = secSec(xs) y

-- removes same elemts
rmSame [] = []
rmSame (x:xs) = x : rmSame (filter (/= x) xs)

fullstopCheck:: String -> Char


-- concatanes the string of second element with that of the first and vice versa
iterator x1 x q [] = []
iterator x1 x q (y:ys) = [(x++[fst(y)!!0] , [snd(y)!!0]++tail(snd(splitLine x1 q)))]++ iterator x1 x q (ys)

fullstopCheck (x:xs) | last(x:xs) == '.' = '.'
fullstopCheck (x:xs) | otherwise = ' '

--"concatanes" the hyphnetaed strings

concat' [] = []
concat' (x:xs) = x ++ concat'(xs)

firstConcat [] y w = ([],[])
firstConcat (x:xs) y w = ([concat'(fst(splitLine1 (x:xs) y)) ++ "-"],[concat'(snd(splitLine1 (x:xs) y)) ++ [fullstopCheck(w)]]) 

-- keeps check the remaing length and concatanates the hyphenated strings together as long as their length is under the limit
totalTuple [] y q w = []
totalTuple x y q w | y < q-1 = rmSame[firstConcat(x) y w] ++ rmSame(totalTuple x (y+1) q w) 
				   | otherwise = rmSame[]

--removes the tail of a filled list , else gives an empty list
removing [] = []
removing x | x == [] = []
		   | otherwise = tail(x)

lineBreaks enHyp y x = [splitLine(x) y] ++ removing(iterator x (fst(splitLine(x) y)) y (totalTuple (secSec enHyp (secondArg x y))  1 ((y) - length(unwords(fst(splitLine(x) y)))) (snd(splitLine(x) y)!!0) ))


-- ========================================================= PART 3 ========================================================= --


--
-- Define a function that inserts a given number of blanks (spaces) into a list of strings and outputs a list of all possible
-- insertions. Only insert blanks between strings and not at the beginning or end of the list (if there are less than two
-- strings in the list then return nothing). Remove duplicate lists from the output.
-- The function should take the number of blanks and the the list of strings as input and return a lists of strings.
--
-- Example:
--    blankInsertions 2 ["A", "creative", "man"]   ==>   [["A", " ", " ", "creative", "man"], ["A", " ", "creative", " ", "man"], ["A", "creative", " ", " ", "man"]]
--
-- Use let/in/where to make the code readable
--


blankInsertions :: Int -> [String] -> [[String]]
-- Function definition here

-- adds spaces recursively
addSpaces [] = []
addSpaces (x:xs) | x /= last(x:xs), x/= " " = (([x] ++ [" "]) ++ xs) : (map (x:) (addSpaces(xs)))
                 | otherwise = map (x:) (addSpaces(xs))

map' [] = []
map' x = addSpaces(x)

-- i made a mapping function and it maps addspaces on the list , and then keeps mapping on the returning list till the number of spaces wanted are achived
test' [] = []
test' x = map' x
test'' x y | y == 1 = map' x
test'' x y | y > 1 = rmSame(concat (map addSpaces (test'' x (y-1))))

blankInsertions y [] = []
blankInsertions 0 x = [x]
blankInsertions y x | y == 1 = test'(x)
					| y > 1 , y /= 0 = test'' x y




-- ========================================================= PART 4 ========================================================= --


--
-- Define a function to score a list of strings based on four factors:
--
--    blankCost: The cost of introducing each blank in the list
--    blankProxCost: The cost of having blanks close to each other
--    blankUnevenCost: The cost of having blanks spread unevenly
--    hypCost: The cost of hyphenating the last word in the list
--
-- The cost of a list of strings is computed simply as the weighted sum of the individual costs. The blankProxCost weight equals
-- the length of the list minus the average distance between blanks (0 if there are no blanks). The blankUnevenCost weight is
-- the variance of the distances between blanks.
--
-- The function should take a list of strings and return the line cost as a double
--
-- Example:
--    lineCost ["He", " ", " ", "who", "controls"]
--        ==>   blankCost * 2.0 + blankProxCost * (5 - average(1, 0, 2)) + blankUnevenCost * variance(1, 0, 2) + hypCost * 0.0
--        ==>   blankCost * 2.0 + blankProxCost * 4.0 + blankUnevenCost * 0.666...
--
-- Use let/in/where to make the code readable
--


---- Do not modify these in the submission ----
blankCost = 1.0
blankProxCost = 1.0
blankUnevenCost = 1.0
hypCost = 1.0
-----------------------------------------------


lineCost :: [String] -> Double
-- Function definition here
count2 :: [String] -> Double -> Double
count2 [] y = y
count2 (x:xs) y | x == " " = count2(xs) (y+1)
				| x /= " " = count2(xs) y

length' [] = 0.0
length' (x:xs) = 1 + length'(xs)

-- This function generate (1,0,2) or any similiar required array for the given string
count [] y = [y]
count (x:xs) y | x /= " " =  (count(xs)(y+1))
			   | x == " " = y : (count xs 0)

average' [] = 0.0
average' x = ((sum(x)) /length'(x))

checkHyphen :: [String] -> Double
checkHyphen x = 0.0
checkHyphen x | last(last x) == '-' = 1.0
			  | otherwise = 0.0

-- firstLast [] = []
-- firstLast [x] = []
-- firstLast xs = tail (init xs)

-- variance :: [Int] -> Double
variance [] = 0.0
variance x = (sum((map (^2) (x)))/length'(x)) - (sum(x)/length'(x))^2


lineCost [] = 0
lineCost x = blankCost * (count2 x 0.0) + blankProxCost * (length'(x) - average'((count(x) 0))) + blankUnevenCost * variance((count(x) 0)) + hypCost * (checkHyphen(x))


-- ========================================================= PART 5 ========================================================= --


--
-- Define a function that returns the best line break in a list of words given a cost function, a hyphenation map and the maximum
-- line width (the best line break is the one that minimizes the line cost of the broken list).
-- The function should take a cost function, a hyphenation map, the maximum line width and the list of strings to split and return
-- a pair of lists of strings as in part 1.
--
-- Example:
--    bestLineBreak lineCost enHyp 12 ["He", "who", "controls"]   ==>   (["He", "who", "cont-"], ["rols"])
--
-- Use let/in/where to make the code readable
-- 

-- bestLineBreak :: ([String] -> Double) -> [(String, [String])] -> Int -> [String] -> ([String], [String])
-- Function definition here


--
-- Finally define a function that justifies a given text into a list of lines satisfying a given width constraint.
-- The function should take a cost function, hyphenation map, maximum line width, and a text string as input and return a list of
-- strings.
--
-- 'justifyText lineCost enHyp 15 text1' should give you the example at the start of the assignment.
--
-- You might find the words and unwords functions useful.
--


-- justifyText :: ([String] -> Double) -> [(String, [String])] -> Int -> String -> [String]
-- Function definition here


-- iterator1 [] = []

iterator222 [] y = []
iterator222 (x:xs) y = (y - x) : iterator222(xs) y

addingSecond [] y = []
addingSecond (x:xs) y | fst(x) == y = snd(x)
					  | otherwise = addingSecond (xs) y

findingIndex :: [Double] -> Double -> Int -> Int
findingIndex [] q y = y
findingIndex (x:xs) q y | x == q = y
					    | otherwise = findingIndex(xs) q (y + 1)

getFst [] = []
getFst (x:xs) = (fst(x)) : getFst(xs)


iterator1 [] = []
iterator1 (x:xs) = length(unwords(fst(x))) : iterator1(xs)

bspaces [] [] = []
bspaces (x:xs) (y:ys) = blankInsertions (x) (y) ++ bspaces (xs) (ys)


compilingFunc x y = bspaces(iterator222(iterator1(lineBreaks enHyp y x)) y) (getFst((lineBreaks enHyp y x)))

prices [] = []
prices (x:xs) = [lineCost(x)] ++ prices(xs)

findingMin y x = findingIndex (prices (compilingFunc x y)) (minimum(prices(compilingFunc x y))) 0

initial enhyp x y = (bspaces(iterator222(iterator1(lineBreaks enHyp y x)) y)(getFst(lineBreaks enHyp y x)))!!(findingMin y x) 

bestLineBreak lineCost enHyp y x  =  ((bspaces(iterator222(iterator1(lineBreaks enHyp y x)) y)(getFst(lineBreaks enHyp y x)))!!(findingMin y x) , addingSecond(lineBreaks enHyp y x)(filter (/=" ") (initial enHyp x y)))


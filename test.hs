type DNA = [Char]
type RNA = [Char]
type Codon = [Char]
type AminoAcid = Maybe String
data DNATree = Node DNA Int DNATree DNATree | Nil deriving (Ord, Show, Eq)

difference1 :: String -> String -> Int -> Int
difference1 [][] count = count
difference1 (x:xs) (y:ys) count | x == y = difference1(xs)(ys) (count - 1)
								| otherwise = difference1 (xs) (ys) count

elem' _ [] = False
elem' x (y : ys) | x == y = True 
				 | otherwise = elem' x ys

functionChoosing:: String -> [String] -> [String]
functionChoosing x [] = []
functionChoosing x (y:ys) | (difference1 x y 8) == 1 = [y] ++ functionChoosing(x) (ys)
						  | otherwise = functionChoosing x (ys)


allPoss [] _ = []
allPoss (x:xs) y = concat[functionChoosing (x) y] ++ allPoss(xs) y


removingSame list1 list2 = manyfilters (map filter (map (/=) list1)) list2
manyfilters [] list2 = list2
manyfilters (f:fs) list2 = manyfilters fs (f list2)


mutationDistance' :: [String] -> String -> Int -> [String] -> Int
mutationDistance' currentStrs _ count [] = -1
mutationDistance' [] _ count _ = 0
mutationDistance' currentStrs final count bank | elem' final (currentStrs) == True = count
						        			  | mutationDistance' (allPoss (currentStrs) (bank)) final (count+1) (removingSame (currentStrs) (bank)) == -1 = -1
						         			  | otherwise = (mutationDistance' (allPoss (currentStrs) (bank)) final (count+1) (removingSame (currentStrs) (bank)))
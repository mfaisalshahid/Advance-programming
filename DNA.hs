--Muhammad faisal Shahid
-- 19100242
-- ---------------------------------------------------------------------
-- DNA Analysis 
-- CS300 Spring 2018
-- Due: 24 Feb 2018 @9pm
-- ------------------------------------Assignment 2------------------------------------
--
-- >>> YOU ARE NOT ALLOWED TO IMPORT ANY LIBRARY
-- Functions available without import are okay
-- Making new helper functions is okay
--
-- ---------------------------------------------------------------------
--
-- DNA can be thought of as a sequence of nucleotides. Each nucleotide is 
-- adenine, cytosine, guanine, or thymine. These are abbreviated as A, C, 
-- G, and T.
--
type DNA = [Char]
type RNA = [Char]
type Codon = [Char]
type AminoAcid = Maybe String

-- ------------------------------------------------------------------------
-- 				PART 1
-- ------------------------------------------------------------------------				

-- We want to calculate how alike are two DNA strands. We will try to 
-- align the DNA strands. An aligned nucleotide gets 3 points, a misaligned
-- gets 2 points, and inserting a gap in one of the strands gets 1 point. 
-- Since we are not sure how the next two characters should be aligned, we
-- try three approaches and pick the one that gets us the maximum score.
-- 1) Align or misalign the next nucleotide from both strands
-- 2) Align the next nucleotide from first strand with a gap in the second     
-- 3) Align the next nucleotide from second strand with a gap in the first    
-- In all three cases, we calculate score of leftover strands from recursive 
-- call and add the appropriate penalty.                                    
score :: DNA -> DNA -> Int
score [][] = 0
score xs [] = length(xs)
score [] ys = length(ys)
score (x:xs) (y:ys) | x == y = maximum [(3 + score(xs)(ys)) , (1 + score(x:xs)(ys)) , (1 + score(xs)(y:ys))]
					| x /= y = maximum [(2 + score(xs)(ys)) , (1 + score(x:xs)(ys)) , (1 + score(xs)(y:ys))]
-- score = undefined

-- -------------------------------------------------------------------------
--				PART 2
-- -------------------------------------------------------------------------
-- Write a function that takes a list of DNA strands and returns a DNA tree. 
-- For each DNA strand, make a separate node with zero score 
-- in the Int field. Then keep merging the trees. The merging policy is:
-- 	1) Merge two trees with highest score. Merging is done by making new
--	node with the smaller DNA (min), and the two trees as subtrees of this
--	tree
--	2) Goto step 1 :)
--

data DNATree = Node DNA Int DNATree DNATree | Nil deriving (Ord, Show, Eq)
makeDNATree :: [DNA] -> DNATree
makeDNATree [] = Nil
makeDNATree x = let
				origList = collectionOfDna(x)
				[(Node p v subtree1 subtree2)] = makingRootNode(findingTree (mappingFunc(origList)) (max'(findingMaximum(mappingFunc(origList)))))
				origList2 = filter (\x -> (x/=subtree1) && (x /= subtree2)) origList
				origList3 = origList2 ++ [(Node p v subtree1 subtree2)] 
				in recursiveFunc (origList3)
				-- makingRootNode (findingTree (mappingFunc(origList2)) (max'(findingMaximum(mappingFunc(origList2)))) : origList2 
-- makeDNATree = undefined

-------------------
calcScores :: DNATree -> DNATree -> ((DNATree,DNATree),Int)
calcScores Nil Nil = ((Nil,Nil),0)
calcScores (Node x q t r) (Node y p v b) = (((Node x q t r),(Node y p v b)) , score(x)(y))

mappingFunc :: [DNATree] -> [((DNATree,DNATree),Int)]
mappingFunc [] = [] 
mappingFunc (x:xs) = map (calcScores (x)) xs ++ mappingFunc(xs)

-- findIndex [] q y = y
-- findIndex (x:xs) q y | x == q = y
-- 					 | otherwise = findIndex(xs) q (y+1)

collectionOfDna :: [DNA] -> [DNATree]
collectionOfDna [] = []
collectionOfDna (x:xs) = [Node x 0 Nil Nil] ++ collectionOfDna(xs)

-- findingMinimum :: [((DNA,DNA),Int)] -> Int
findingMaximum [] = []
findingMaximum (x:xs) = [snd(x)] ++ findingMaximum(xs)

max' [] = 0
max' x = maximum(x)

findingTree [] q = []
findingTree (x:xs) q | snd(x) == q = [x]
					  | otherwise = findingTree(xs) q

minOfDna x | fst(x) <= snd(x) = fst(x)
		   | otherwise = snd(x)

maxOfDna x | fst(x) >= snd(x) = fst(x)
		   | otherwise = snd(x)

leftMinTree x = Node (minOfDna x) 0 Nil Nil

rightMaxTree x = Node (maxOfDna x) 0 Nil Nil

makingRootNode [] = []
makingRootNode [(((Node x q t r),(Node y p v b)), e)] | p < q = [Node (min x y) (e) (Node y p v b) (Node x q t r)]
													  | otherwise = [Node (min x y) (e) (Node x q t r) (Node y p v b)]


-- filtering = filter (/= fst(x)) collectionOfDna x

recursiveFunc :: [DNATree] -> DNATree
recursiveFunc [x] = x
recursiveFunc x = let
				[(Node p v subtree1 subtree2)] = makingRootNode(findingTree (mappingFunc(x)) (max'(findingMaximum(mappingFunc(x)))))
				origList2 = filter (\x -> x /=subtree1 && x /= subtree2) x
				origList3 = origList2 ++ [(Node p v subtree1 subtree2)] 
				in recursiveFunc (origList3)

--------------------
-- -------------------------------------------------------------------------
--				PART 3
-- -------------------------------------------------------------------------

-- Even you would have realized it is hard to debug and figure out the tree
-- in the form in which it currently is displayed. Lets try to neatly print 
-- the DNATree. Each internal node should show the 
-- match score while leaves should show the DNA strand. In case the DNA strand 
-- is more than 10 characters, show only the first seven followed by "..." 
-- The tree should show like this for an evolution tree of
-- ["AACCTTGG","ACTGCATG", "ACTACACC", "ATATTATA"]
--
-- 20
-- +---ATATTATA
-- +---21
--     +---21
--     |   +---ACTGCATG
--     |   +---ACTACACC
--     +---AACCTTGG
--
-- Make helper functions as needed. It is a bit tricky to get it right. One
-- hint is to pass two extra string, one showing what to prepend to next 
-- level e.g. "+---" and another to prepend to level further deep e.g. "|   "
-- data DNATree = Node DNA Int DNATree DNATree | Nil deriving (Ord, Show, Eq)

draw :: DNATree -> [Char]
draw (Node string score Nil Nil) = " +--- " ++ string
draw (Node string score node1 node2) | node1 /= Nil && node2 /= Nil = show(score) ++ "\n" ++ (traversalLeft(node1) 1) ++ (traversalRight(node2) 1)
									 | node1 /= Nil && node2 == Nil = show(score) ++ "\n" ++ (traversalLeft(node1) 1)
									 | node1 == Nil && node2 /= Nil = show(score) ++ "\n" ++ (traversalRight(node2) 1)
traversalLeft :: DNATree -> Int -> [Char]
traversalLeft Nil _ = ""
traversalLeft (Node string score Nil Nil) depth = (concat(replicate(depth-1) ("|   "))) ++"+--- " ++ string ++ "\n"
traversalLeft (Node string score node1 node2) depth = (concat(replicate(depth-1) ("|   "))) ++ "+--- " ++ show(score) ++ "\n" ++ (traversalLeft(node1) (depth+1)) ++ (traversalRight(node2)(depth+1))
												
traversalRight :: DNATree -> Int -> [Char]
traversalRight Nil _ = ""
traversalRight (Node string score Nil Nil) depth = (concat(replicate(depth-1) ("|   "))) ++"+--- " ++ string ++ "\n"
traversalRight (Node string score node1 node2) depth = (concat(replicate(depth-1) ("|   "))) ++ "+--- " ++ show(score) ++ "\n" ++ (traversalLeft(node2) (depth+1)) ++ (traversalRight(node1)(depth+1))

-- drawing :: DNATree -> [Char]
-- drawing (Node string score node1 node2) = "|" ++ show (score) ++ " +--- " ++ string
-- ---------------------------------------------------------------------------
--				PART 4
-- ---------------------------------------------------------------------------
--
--
-- Our score function is inefficient due to repeated calls for the same 
-- suffixes. Lets make a dictionary to remember previous results. First you
-- will consider the dictionary as a list of tuples and write a lookup
-- function. Return Nothing if the element is not found. Also write the 
-- insert function. You can assume that the key is not already there.
type Dict a b = [(a,b)]

lookupDict :: (Eq a) => a -> Dict a b -> Maybe b
lookupDict x [] = Nothing
lookupDict x (y:ys) | x == fst(y) = Just (snd(y))
					| otherwise = Nothing

insertDict :: (Eq a) => a -> b -> (Dict a b)-> (Dict a b)
insertDict x y list = [(x,y)] ++ list

-- We will improve the score function to also return the alignment along
-- with the score. The aligned DNA strands will have gaps inserted. You
-- can represent a gap with "-". You will need multiple let expressions 
-- to destructure the tuples returned by recursive calls.

-- alignment :: String -> String -> ((String, String), Int)
-- alignment [][] = ()
-- alignment (x:xs) (y:ys) = alignment(((xs)(ys)),score(xs)(ys)) , alignment(x:xs)("-" ++ ys),score(x:xs)("-" ++ ys) , alignment("-"++xs)(y:ys),score("-"++xs)(y:ys)]

calculating :: String -> String -> [((String, String), Int)]
calculating [][] = [(("",""),0)]
calculating (x:xs) (y:ys) = let
                               a = [((x:xs,y:ys),(if (x == y) then (3+score(xs)(ys)) else (2+score(xs)(ys)))) ]
                               b = [(("-"++x:xs),y:ys),(1+score(x:xs)(ys)) ]
                               c = [(x:xs,("-"++y:ys)),(1+score(xs)(y:ys))]
                            in maximum' (concat(a , b , c))
-- makingTempDict [] [] score = ()
-- makingTempDict (x:xs) (y:ys) score | x == y = ((xs,ys),score(xs)(xs)) , ) 

maximum' [] = []
maximum' [] = [(("",""),0)]
maximum (x:xs)   
-- We will now pass a dictionary to remember previously calculated scores 
-- and return the updated dictionary along with the result. Use let 
-- expressions like the last part and pass the dictionary from each call
-- to the next. Also write logic to skip the entire calculation if the 
-- score is found in the dictionary. You need just one call to insert.
type ScoreDict = Dict (DNA,DNA) Int

scoreMemo :: (DNA,DNA) -> ScoreDict -> (ScoreDict,Int)
scoreMemo = undefined
-- In this part, we will use an alternate representation for the 
-- dictionary and rewrite the scoreMemo function using this new format.
-- The dictionary will be just the lookup function so the dictionary 
-- can be invoked as a function to lookup an element. To insert an
-- element you return a new function that checks for the inserted
-- element and returns the old dictionary otherwise. You will have to
-- think a bit on how this will work. An empty dictionary in this 
-- format is (\_->Nothing)

type Dict2 a b = a->Maybe b
insertDict2 :: (Eq a) => a -> b -> (Dict2 a b)-> (Dict2 a b)
insertDict2 = undefined

type ScoreDict2 = Dict2 (DNA,DNA) Int

scoreMemo2 :: (DNA,DNA) -> ScoreDict2 -> (ScoreDict2,Int)
scoreMemo2 = undefined

-- ---------------------------------------------------------------------------
-- 				PART 5
-- ---------------------------------------------------------------------------

-- Now, we will try to find the mutationDistance between two DNA sequences.
-- You have to calculate the number of mutations it takes to convert one 
-- (start sequence) to (end sequence). You will also be given a bank of 
-- sequences. However, there are a couple of constraints, these are as follows:

-- 1) The DNA sequences are of length 8
-- 2) For a sequence to be a part of the mutation distance, it must contain 
-- "all but one" of the neuclotide bases as its preceding sequence in the same 
-- order AND be present in the bank of valid sequences
-- 'AATTGGCC' -> 'AATTGGCA' is valid only if 'AATTGGCA' is present in the bank
-- 3) Assume that the bank will contain valid sequences and the start sequence
-- may or may not be a part of the bank.
-- 4) Return -1 if a mutation is not possible

	
-- mutationDistance "AATTGGCC" "TTTTGGCA" ["AATTGGAC", "TTTTGGCA", "AAATGGCC" "TATTGGCC", "TTTTGGCC"] == 3
-- mutationDistance "AAAAAAAA" "AAAAAATT" ["AAAAAAAA", "AAAAAAAT", "AAAAAATT", "AAAAATTT"] == 2

mutationDistance :: DNA -> DNA -> [DNA] -> Int
mutationDistance var1 var2 list = mutationDistance' [var1] (var2) 0 list


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


-- ---------------------------------------------------------------------------
-- 				PART 6
-- ---------------------------------------------------------------------------
--
-- Now, we will write a function to transcribe DNA to RNA. 
-- The difference between DNA and RNA is of just one base i.e.
-- instead of Thymine it contains Uracil. (U)
--
transcribeDNA :: DNA -> RNA
transcribeDNA [] = []
transcribeDNA (x:xs) | x == 'T' = 'U' : transcribeDNA xs 
     				 | otherwise = x : transcribeDNA xs

-- Next, we will translate RNA into proteins. A codon is a group of 3 neuclotides 
-- and forms an aminoacid. A protein is made up of various amino acids bonded 
-- together. Translation starts at a START codon and ends at a STOP codon. The most
-- common start codon is AUG and the three STOP codons are UAA, UAG and UGA.
-- makeAminoAcid should return Nothing in case of a STOP codon.
-- Your translateRNA function should return a list of proteins present in the input
-- sequence. 
-- Please note that the return type of translateRNA is [String], you should convert
-- the abstract type into a concrete one.
-- You might wanna use the RNA codon table from 
-- https://www.news-medical.net/life-sciences/RNA-Codons-and-DNA-Codons.aspx
-- 
--

makeAminoAcid :: Codon -> AminoAcid
makeAminoAcid [] = Nothing
makeAminoAcid x | (x == "UAA" || x=="UAG" || x=="UGA") = Nothing
makeAminoAcid x | (x == "UUU" || x=="UUC") = Just "Phe"  
				| (x == "UUA" || x=="UUG" || x=="CUU" || x=="CUC") = Just "Leu" 
				| (x == "AUU" || x=="AUC" || x=="AUA") = Just "Ile"
				| (x =="AUG") = Just "Met"
				| (x == "GUU" || x=="GUC" || x=="GUA" || x=="GUG") = Just "Val"
				| (x == "UCU" || x=="UCC" || x=="UCA" || x=="UCG" || x=="AGU" || x=="AGC") = Just "Ser"
				| (x == "CCU" || x=="CCC" || x=="CCA" || x=="CCG") = Just "Pro"
				| (x == "ACU" || x=="ACC" || x=="ACA" || x=="ACG") = Just "Thr"
 				| (x == "GCU" || x=="GCC" || x=="GCA" || x=="GCG") = Just "Ala"
 				| (x == "UAU" || x=="UAC")  = Just "Tyr"
 				| (x == "CAU" || x=="CAC")  = Just "His"
 				| (x == "CAA" || x=="CAG")  = Just "Gln"
 				| (x == "AAU" || x=="AAC")  = Just "Asn"
 				| (x == "AAA" || x=="AAG")  = Just "lys"
 				| (x == "GAU" || x=="GAC")  = Just "Asp"
 				| (x == "GAA" || x=="GAG")  = Just "Glu"
 				| (x == "UGU" || x=="UGC")  = Just "Cys"
 				| (x == "UGG")  = Just "Trp"
 				| (x == "CGU" || x=="CGC" || x=="CGA" || x=="CGG" || x=="AGG" || x=="AGA") = Just "Arg"
 				| (x == "GCU" || x=="GCC" || x=="GCA" || x=="GCG") = Just "Ala"
 				| (x == "GGU" || x=="GGC" || x=="GGA" || x=="GGG") = Just "Gly"
 				| otherwise = Nothing

translateRNA :: RNA -> [String]
translateRNA [] = []
translateRNA (x:x1:x2:xs) | ([x,x1,x2]) == "AUG" = [concat("Met" : startingFunc(xs))]
						  | otherwise = translateRNA(xs)

startingFunc :: RNA -> [String]
startingFunc [] = []
startingFunc (x:x1:x2:xs) | ([x,x1,x2] == "UAA" || [x,x1,x2]=="UAG" || [x,x1,x2]=="UGA") = (map (\(Just i)->i) (filter (/=Nothing) [makeAminoAcid [x,x1,x2]])) ++ translateRNA(xs)
						  | otherwise = (map (\(Just i)->i) (filter (/=Nothing) [makeAminoAcid [x,x1,x2]])) ++ startingFunc(xs)




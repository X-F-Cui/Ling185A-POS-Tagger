module FinalProject02 where

import Control.Applicative(liftA, liftA2, liftA3)
import Data.List

import ProbSLG
import Brown

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- A
follows :: Ord sy => ProbSLG sy -> sy -> [(sy, Double)]
follows m x =
	let (syms, i, f, bigs) = m in
	map (\(s1, s2, prob) -> (s2, prob)) (filter (\(s1, s2, prob) -> s1 == x) bigs)

-- B
bigrams :: [a] -> [(a,a)]
bigrams l =
	case l of 
	[x] -> []
	(x : y : rest) -> [(x, y)] ++ bigrams (y : rest)

-- C
lastSymbol :: Ord sy => [sy] -> sy
lastSymbol l =
	case l of
	[x] -> x
	(x : rest) -> lastSymbol rest


findProb :: Ord sy => [(sy, Double)] -> sy -> Double
findProb l s =
	case l of
	[(x, prob)] -> if (x == s) then prob else 0
	(x, prob) : rest -> if (x == s) then prob else findProb rest s


findTransProbs :: Ord sy => ProbSLG sy -> [(sy, sy)] -> Double
findTransProbs m l =
	let (syms, i, f, bigs) = m in
	case l of
	[(x1, x2)] -> findProb (follows m x1) x2
	(x1, x2) : rest -> if ((findProb (follows m x1) x2) == 0) then 0 
		                   else (findProb (follows m x1) x2) * (findTransProbs m rest)


valP :: Ord sy => ProbSLG sy -> [sy] -> Double
valP m string =
	let (syms, i, f, bigs) = m in
	case string of
	[x] -> findProb i x * findProb f x
	(x : rest) -> findProb i x * findTransProbs m (bigrams string) * findProb f (lastSymbol string)

-- D
countElemInCorp :: (Ord a, Eq a) => Corpus a -> a -> Int
countElemInCorp corp x =
	case corp of
	{[] -> 0;
	(sentence : tail) -> case sentence of {[] -> countElemInCorp tail x;
	                                       (s : rest) -> (if (s == x) then 1 else 0) + countElemInCorp (rest:tail) x}}


countElemInList :: Eq a => [a] -> a -> Int
countElemInList l x =
	case l of
	[] -> 0
	(s : rest) -> (if (s == x) then 1 else 0) + countElemInList rest x


-- for syms
addAllSyms :: (Ord a, Eq a) => Corpus a -> [a]
addAllSyms corp =
	case corp of
	{[] -> [];
	 (st : tail) -> case st of
		           {[] -> addAllSyms tail;
		           (x : rest) -> [x] ++ addAllSyms (rest : tail)}}


-- for i
firstElement :: (Ord a, Eq a) => Corpus a -> [a]
firstElement corp =
	case corp of
	[] -> []
	((s : rest) : tail) -> [s] ++ firstElement tail


-- for f
lastElement :: (Ord a, Eq a) => Corpus a -> [a]
lastElement corp =
	case corp of
	{[] -> [];
	(sentence : tail) -> case sentence of {[x] -> [x] ++ lastElement tail; (x : rest) -> lastElement (rest : tail)}}


-- for bigs
findNext :: (Ord a, Eq a) => Corpus a -> a -> [a]
findNext corp x =
	case corp of
	{[] -> [];
	(sentence : tail) -> case sentence of {[] -> findNext tail x; [a] -> findNext tail x; 
	                     (a : b : rest) -> if (a == x) then ([b] ++ (findNext ((b:rest):tail) x)) else (findNext ((b:rest):tail) x)}}


buildProbSLG :: (Ord a, Eq a) => Corpus a -> ProbSLG a
buildProbSLG corp =
	let syms = nub (addAllSyms corp) in
	let i = map (\x -> (x, divide (countElemInList (firstElement corp) x) (length corp))) syms in
	let f = map (\x -> (x, divide (countElemInList (lastElement corp) x) (countElemInCorp corp x))) syms in
	let bigs = liftA2 (\x -> \y -> (x, y, divide (countElemInList (findNext corp x) y) (countElemInCorp corp x))) syms syms in
    (syms, i, f, bigs)

-- E
taggedWordstoTags1 :: Sentence TaggedWord -> Sentence String
taggedWordstoTags1 sentence =
	map getTag sentence


taggedWordstoTags2 :: Corpus TaggedWord -> Corpus String
taggedWordstoTags2 corp =
	map taggedWordstoTags1 corp


posProbSLG :: Corpus TaggedWord -> ProbSLG String
posProbSLG corp = 
	buildProbSLG (taggedWordstoTags2 corp)


-- F
tagN = ["nn", "nns", "np", "nn-tl", "np-hl", "nn-hl", "hp-tl", "pps", "vbg", "ppss", "vbg-tl", "nns-hl"]
tagBe = ["ber", "bedz", "ben", "bez", "be"]
tagHave = ["hv", "hvz", "hvd"]
tagV = ["vbd", "vbn", "vb", "vbz", "vb-hl", "dod", "vbn-hl", "do"]
tagP = ["in", "cs", "in-tl", "in-hl"]
tagConj = ["cc", "cc-tl", "wrb"]
tagD = ["at", "cd", "nn$", "pp$", "dts", "abn", "dti", "abx", "pn", "dt", "np$", "ppo", "nn$-tl"]
tagAdj = ["jj-tl", "jj", "ap", "jjs", "jjt"]
tagAdv = ["rb", "ql", "nr", "rbr"]
tagT =["md", "md-hl"]
tagNeg = ["*"]
tagTo = ["to"]
tagWH = ["wps", "wdt"]
tagPunc = [",-hl", "(-hl", ")-hl", "--", ",", "``", ":"]


retag1 :: TaggedWord -> TaggedWord
retag1 x =
	if (elem (getTag x) tagN) then (TaggedWord (getWord x, "N")) 
		else if (elem (getTag x) tagBe) then (TaggedWord (getWord x, "Be"))
			else if (elem (getTag x) tagHave) then (TaggedWord (getWord x, "Have"))
				else if (elem (getTag x) tagV) then (TaggedWord (getWord x, "V"))
					else if (elem (getTag x) tagP) then (TaggedWord (getWord x, "P"))
						else if (elem (getTag x) tagConj) then (TaggedWord (getWord x, "Conj"))
							else if (elem (getTag x) tagD) then (TaggedWord (getWord x, "D"))
								else if (elem (getTag x) tagAdj) then (TaggedWord (getWord x, "Adj"))
									else if (elem (getTag x) tagAdv) then (TaggedWord (getWord x, "Adv"))
										else if (elem (getTag x) tagT) then (TaggedWord (getWord x, "T"))
											else if (elem (getTag x) tagNeg) then (TaggedWord (getWord x, "Neg"))
												else if (elem (getTag x) tagTo) then (TaggedWord (getWord x, "To"))
													else if (elem (getTag x) tagWH) then (TaggedWord (getWord x, "WH"))
														else (TaggedWord (getWord x, "Punc"))


retag2 :: Sentence TaggedWord -> Sentence TaggedWord
retag2 sentence =
	filter (\x -> getTag x /= "Punc") (map retag1 sentence)


sanitize :: Corpus TaggedWord -> Corpus TaggedWord
sanitize corp =
	map retag2 corp

-- G
findTags1 :: Sentence TaggedWord -> String -> [String]
findTags1 sentence s =
	case sentence of
	[] -> []
	(x : rest) -> (if (getWord x == s) then [getTag x] else []) ++ findTags1 rest s


findTags2 :: Corpus TaggedWord -> String -> [String]
findTags2 corp s =
	case corp of
	[] -> []
	(sentence : rest) -> findTags1 sentence s ++ findTags2 rest s


countTimes :: [String] -> [(String, Int)]
countTimes list =
	map (\x -> (x, countElemInList list x)) (nub list)


compareSecond (tag1, count1) (tag2, count2) = compare count1 count2

mostFrequentString :: (Ord a, Eq a) => [(String, a)] -> String
mostFrequentString l =
	if (length l == 0) then "" else
	    let (tag, count) = maximumBy compareSecond l in tag


mostFrequentTag :: Corpus TaggedWord -> String -> String
mostFrequentTag corp s =
	mostFrequentString (countTimes (findTags2 corp s))

-- H
mostFrequentI :: ProbSLG String -> String
mostFrequentI (syms, i, f, bigs) = mostFrequentString i


firstTag :: Corpus TaggedWord -> [String] -> String
firstTag corp sentence = 
	let x : rest = sentence in
	    if (length (findTags2 corp x) > 0) then mostFrequentTag corp x
	    	else mostFrequentI (posProbSLG corp)


reverseSentence :: [String] -> [String]
reverseSentence sentence =
	case sentence of
	[] -> []
	(s : rest) -> reverseSentence rest ++ [s]


update1Helper :: Corpus TaggedWord -> (Sentence TaggedWord, Double) -> String -> (Sentence TaggedWord, Double)
update1Helper corp (s, prob) x =
	(s ++ [TaggedWord (x, mostFrequentTag corp x)], prob)


update1 :: Corpus TaggedWord -> [(Sentence TaggedWord, Double)] -> String -> [(Sentence TaggedWord, Double)]
update1 corp list x =
	map (\s -> update1Helper corp s x) list


lastWord :: Sentence TaggedWord -> TaggedWord
lastWord sentence =
	case sentence of
	[x] -> x
	(x: rest) -> lastWord rest


update2Helper :: Corpus TaggedWord -> (Sentence TaggedWord, Double) -> String -> [(Sentence TaggedWord, Double)]
update2Helper corp (s, prob) x =
	let slg = posProbSLG corp in
	let (syms, i, f, bigs) = slg in 
	map (\(y, prob') -> (s ++ [TaggedWord (x, y)], prob * prob' * 0.5)) (follows slg (getTag (lastWord s)))


update2 :: Corpus TaggedWord -> [(Sentence TaggedWord, Double)] -> String -> [(Sentence TaggedWord, Double)]
update2 corp list x =
	case list of 
	[] -> []
	(tuple : rest) -> (update2Helper corp tuple x) ++ (update2 corp rest x)


tagHelper :: Corpus TaggedWord -> [String] -> [(Sentence TaggedWord, Double)]
tagHelper corp sentence = 
	let slg = posProbSLG corp in
	let (syms, i, f, bigs) = slg in 
	    case sentence of
	    {[x] -> [([TaggedWord (x, firstTag corp sentence)], 1)];
	    (x : prev : rest) -> if (length (findTags2 corp x) > 0) then (update1 corp (tagHelper corp (prev:rest)) x)
	                           else (update2 corp (tagHelper corp (prev:rest)) x)
	                       }


tag :: Corpus TaggedWord -> [String] -> [(Sentence TaggedWord, Double)]
tag corp sentence = tagHelper corp (reverseSentence sentence)

-- I
tagBest :: Corpus TaggedWord -> [String] -> Sentence TaggedWord
tagBest corp sentence =
	let (taggedSentence, prob) = maximumBy compareSecond (tag corp sentence) in taggedSentence


-- J
testSentence :: Sentence TaggedWord -> Sentence TaggedWord -> Int
testSentence actual predict =
	case actual of
	{[] -> 0;
	(s : rest) -> case predict of {[] -> 0; (x : tail) -> (if (getTag x == getTag s) then 1 else 0) + testSentence rest tail}}


testCorp :: Corpus TaggedWord -> Corpus TaggedWord -> Int
testCorp corpActual corpPredict = 
	sum (zipWith (\x y -> testSentence x y) corpActual corpPredict)


stripTag1 :: Sentence TaggedWord -> [String]
stripTag1 sentenceActual = map getWord sentenceActual


stripTag2 :: Corpus TaggedWord -> [[String]]
stripTag2 corpActual = map stripTag1 corpActual


tagCorp :: Corpus TaggedWord -> Corpus TaggedWord -> Corpus TaggedWord
tagCorp corpTrain corpTest =
	map (tagBest corpTrain) (stripTag2 corpTest)


corpReduction :: Corpus TaggedWord -> Corpus TaggedWord
corpReduction corp = map (take 5) corp


testAccuracy :: Corpus TaggedWord -> Corpus TaggedWord -> Double
testAccuracy corpTrain corpTest = 
	let reducedCorp = corpReduction corpTest in
	let correct = testCorp reducedCorp (tagCorp corpTrain reducedCorp) in
	(divide correct (sum (map length reducedCorp)))
















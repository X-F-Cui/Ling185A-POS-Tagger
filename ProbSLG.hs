module ProbSLG  where

type ProbSLG sy = ([sy], [(sy, Double)], [(sy, Double)], [(sy, sy, Double)])

-- An example PLSG
g1 :: ProbSLG String
g1 =
    ( [ "the", "very", "fuzzy", "cat", "on", "mat"]
    , [ ("the", 1.0) ]
    , [ ("cat", 0.8), ("mat", 0.8) ]
    , [ ("the", "cat", 0.3), ("the", "very", 0.2), ("the", "fuzzy", 0.2), ("the", "mat", 0.3)
      , ("very", "very", 0.3), ("very", "fuzzy", 0.7), ("fuzzy", "cat", 0.6), ("fuzzy", "mat", 0.4) 
      , ("cat", "on", 0.2), ("mat", "on", 0.2), ("on", "the", 1.0)]
    )

-------------------------------------------------------------------------------
-- Types for corpora
-------------------------------------------------------------------------------

-- Sentences are lists of symbols
type Sentence a = [a]

-- Corpora are lists of sentences (i.e. lists of lists of symbols)
type Corpus a = [Sentence a]

-- Tagged words are pairs: (word, part-of-speech tag)
newtype TaggedWord = TaggedWord (String, String)
                   deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- Some simple helper functions
-------------------------------------------------------------------------------

-- Divides integers without rounding
divide :: (Integral a, Integral b, Fractional c) => a -> b -> c
divide x y = (fromIntegral x) / (fromIntegral y)

-- Gets just the tag out of a tagged-word pair
getTag :: TaggedWord -> String
getTag (TaggedWord (word, tag)) = tag

-- Gets just the word out of a tagged-word pair
getWord :: TaggedWord -> String
getWord (TaggedWord (word, tag)) = word

-------------------------------------------------------------------------------
-- Some easy-to-work-with corpora
-------------------------------------------------------------------------------

corpus1 :: Corpus Bool
corpus1 =
    [ [True, True, True]
    , [True, False]
    , [True, False]
    , [False, True]
    , [False, False, True]
    ]

corpus2 :: Corpus String
corpus2 =
    [ ["D", "N"]
    , ["D", "N"]
    , ["D", "N"]
    , ["D", "N"]
    , ["D", "N"]
    , ["D", "N"]
    , ["D", "N"]
    , ["D", "N"]
    , ["D", "N"]
    , ["D", "Adj", "N"]
    , ["D", "Adj", "N"]
    , ["D", "Adv", "Adj", "N"]
    , ["D", "Adv", "Adj", "N"]
    , ["D", "Adv", "Adj", "N"]
    , ["D", "Adv", "Adv", "Adj", "N"]
    , ["D", "N", "P", "D", "N"]
    , ["D", "N", "P", "D", "N"]
    , ["D", "N", "P", "D", "Adj", "N"]
    , ["D", "Adj", "N", "P", "D", "N"]
    , ["D", "Adj", "N", "P", "D", "Adv", "Adj", "N"]
    ]

corpus3 :: Corpus TaggedWord
corpus3 =
    [ [TaggedWord ("the", "D"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("mat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("mat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("mat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("mat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("mat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("fuzzy", "Adj"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("fuzzy", "Adj"), TaggedWord ("mat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("very", "Adv"), TaggedWord ("fuzzy", "Adj"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("very", "Adv"), TaggedWord ("fuzzy", "Adj"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("very", "Adv"), TaggedWord ("fuzzy", "Adj"), TaggedWord ("mat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("very", "Adv"),TaggedWord ("very", "Adv")
      , TaggedWord ("fuzzy", "Adj"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("cat", "N"), TaggedWord ("on", "P")
      , TaggedWord ("the", "D"), TaggedWord ("mat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("mat", "N"), TaggedWord ("on", "P")
      , TaggedWord ("the", "D"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("cat", "N"), TaggedWord ("on", "P")
      , TaggedWord ("the", "D"), TaggedWord ("fuzzy", "Adj"), TaggedWord ("mat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("fuzzy", "Adj"), TaggedWord ("cat", "N"), TaggedWord ("on", "P")
      , TaggedWord ("the", "D"), TaggedWord ("mat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("fuzzy", "Adj"), TaggedWord ("mat", "N"), TaggedWord ("on", "P")
      , TaggedWord ("the", "D"), TaggedWord ("very", "Adv"), TaggedWord ("fuzzy", "Adj"), TaggedWord ("cat", "N")]
    ]

corpus4 :: Corpus TaggedWord
corpus4 =
    [ [TaggedWord ("the", "C"), TaggedWord ("the", "A")]
    , [TaggedWord ("the", "A"), TaggedWord ("the", "A")]
    , [TaggedWord ("the", "A"), TaggedWord ("the", "A")]
    , [TaggedWord ("the", "A"), TaggedWord ("the", "A")]
    , [TaggedWord ("the", "A"), TaggedWord ("the", "A")]
    , [TaggedWord ("the", "B"), TaggedWord ("the", "B")]
    , [TaggedWord ("the", "B"), TaggedWord ("the", "B")]
    , [TaggedWord ("the", "B"), TaggedWord ("the", "B")]
    , [TaggedWord ("the", "B"), TaggedWord ("the", "B")]
    , [TaggedWord ("the", "B"), TaggedWord ("the", "B")]
    ]

corpus5 :: Corpus TaggedWord
corpus5 =
    [ [TaggedWord ("the", "A"), TaggedWord ("the", "A")]
    , [TaggedWord ("the", "A"), TaggedWord ("the", "A")]
    , [TaggedWord ("the", "A"), TaggedWord ("the", "A")]
    , [TaggedWord ("cat", "A"), TaggedWord ("cat", "A")]
    , [TaggedWord ("cat", "A"), TaggedWord ("cat", "A")]
    , [TaggedWord ("the", "B"), TaggedWord ("the", "B")]
    , [TaggedWord ("the", "B"), TaggedWord ("the", "B")]
    , [TaggedWord ("cat", "B"), TaggedWord ("cat", "B")]
    , [TaggedWord ("cat", "B"), TaggedWord ("cat", "B")]
    , [TaggedWord ("cat", "B"), TaggedWord ("cat", "B")]
    ]


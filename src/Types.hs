{-
    Author: Matúš Škuta (xskuta04)
    Project: VUT FIT FLP 1. Project (Functional) - SIMPLIFY-BKG
    Year: 2021
-}

module Types ( BKG(..), RULE(..) ) where 

import Data.List ( intercalate )

-- Split string into characters, and join them on delimeter
join :: String -> Char -> String
join [] _ = []
join [x] _ = [x]
join (x:xs) d = x : d : join xs d

-- Generate string from list of RULES
formatRules :: [RULE] -> String
formatRules [] = []
--formatRules [x] = show x
formatRules (x:xs) = show x ++ ['\n'] ++ formatRules xs

-- Data representing rule as Char->String, eg. A->aA
data RULE = RULE Char String deriving (Eq)

-- Data representing BKG collection (non_terminals, terminals, starting_symbol, list_of_rules)
data BKG = BKG String String Char [RULE]

-- Define how to show BKG
instance Show BKG where
    show (BKG n t s r) = "\n" `intercalate` [join n ',', join t ',', [s], formatRules r]

-- Define how to show RULE
instance Show RULE where 
    show (RULE a b) = "->" `intercalate` [[a], b]

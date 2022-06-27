{-
    Author: Matúš Škuta (xskuta04)
    Project: VUT FIT FLP 1. Project (Functional) - SIMPLIFY-BKG
    Year: 2021
-}

module Minimize ( simplify1, simplify2 ) where

import Types ( BKG(..),RULE (RULE) )
import ParseInput ( strConsistOnlyOf )
import Data.List ( sort, intersect )

-- Algorithm 4.1, remove rules that will never terminate, return Left BKG, when L(G) is non empty, right when is empty
simplify1 :: BKG -> String -> Int -> Either BKG BKG
simplify1 bkg@(BKG n t s r) ni_1 i 
    --End after n + 1 iterations, or when Ni-1 == Ni
    | i >= length n + 2 || sort ni_1 == sort ni =
        -- Check if BKG is non empty
        if s `elem` ni then
            -- Non empty
            Left bkg_ret
        -- Empty BKG
        else
            Right bkg_ret
    | otherwise                                 = simplify1 bkg ni $ i + 1
    where 
        ni = ni_1 ++ getNonTermFromRules r (ni_1 ++ t ++ ['#'])
        bkg_ret = BKG (uniq (ni ++ [s]) []) t s (filterRules r (ni ++ t ++ ['#']))

-- Get non terminals, from rules, which second part consist only of given symbols
getNonTermFromRules :: [RULE] -> String -> String
getNonTermFromRules [] _ = []
getNonTermFromRules (rule@(RULE a b):xs) elems 
    | strConsistOnlyOf b elems  = a : getNonTermFromRules xs elems
    | otherwise                 = getNonTermFromRules xs elems

-- Return list of rules, that consist only from given symbols
filterRules :: [RULE] -> String -> [RULE]
filterRules [] _ = []
filterRules (rule@(RULE a b):xs) elems 
    | a `elem` elems && strConsistOnlyOf b elems    = rule : filterRules xs elems
    | otherwise                                     = filterRules xs elems

-- Algorithm 4.2, remove rules unreachable from starting symbol
simplify2 :: String -> Int -> BKG -> BKG
simplify2 [] 0 bkg@(BKG _ _ s _) = simplify2 [s] 1 bkg
simplify2 vi_1 i bkg@(BKG n t s r)
    -- End when Vi-1 == Vi
    |sort vi_1 == sort vi   = BKG (vi `intersect` n) (vi `intersect` t) s (filterRules r vi)
    | otherwise             = simplify2 vi (i + 1) bkg
    where 
        vi = uniq (vi_1 ++ getNTFromRules r vi_1) []

-- Get terminals and non terminals, from second part of rules, when first part of rule is one of given symbols 
getNTFromRules :: [RULE] -> String -> String
getNTFromRules [] _ = []
getNTFromRules (rule@(RULE a b):xs) elems
    | a `elem` elems    = b ++ getNTFromRules xs elems
    | otherwise         = getNTFromRules xs elems

-- Construct unique list of values
uniq :: Eq a => [a] -> [a] -> [a]
uniq [] elems = elems
uniq (x:xs) elems
    | x `elem` elems    = uniq xs elems
    |otherwise          = uniq xs (elems ++ [x])
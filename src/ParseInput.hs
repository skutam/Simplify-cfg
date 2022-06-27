{-
    Author: Matúš Škuta (xskuta04)
    Project: VUT FIT FLP 1. Project (Functional) - SIMPLIFY-BKG
    Year: 2021
-}

module ParseInput (parseBKG, strConsistOnlyOf) where

import Types ( BKG(..), RULE (RULE) )
import Data.List ( sort )

-- Generate BKG data, from list of strings, return BKG or error string
parseBKG :: [String] -> Either BKG String 
-- Check if first line consist only of Non Terminals
parseBKG (n:t:s:r) = case parseSymbols n ['A'..'Z'] [] of
        Right err -> Right err
        -- Check if second line consist only of Terminals
        Left non_terminals -> case parseSymbols t ['a'..'z'] [] of
            Right err -> Right err
            -- Check if third line is start symbol, that is one of the loaded non terminals
            Left terminals -> case parseStartSymbol s non_terminals of
                Right err -> Right err
                -- Check if each line of rule, consist only of terminals and non terminals that we had loaded
                Left start_symbol -> case parseRules r non_terminals terminals [] of
                    Right err -> Right err
                    -- When rules do not repeat itself, return created BKG, otherwise return error
                    Left rules -> if checkUniqueness rules [] then
                            Left $ BKG non_terminals terminals start_symbol rules
                        else
                            Right "Rules are not unique"
parseBKG _ = Right "Invalid input format"

-- Parse list of symbols divided by comma, and check if symbols consist only of given symbols, and values are unique
parseSymbols :: String -> String -> String -> Either String String
parseSymbols [] _ []      = Left []
parseSymbols [x] elems str
    | x `elem` elems && x `notElem` str = Left $ str ++ [x]
    | otherwise                         = Right "Invalid input format"
parseSymbols (x:',':xs) elems str
    | x `elem` elems && x `notElem` str = parseSymbols xs elems $ str ++ [x]
    | otherwise                         = Right "Invalid input format"
parseSymbols _ _ _ = Right "Invalid input format"

-- Check if start symbol is only 1 char, and if it is one of given symbols
parseStartSymbol :: String -> String -> Either Char String
parseStartSymbol [s] elems
    | s `elem` elems    = Left s
    | otherwise         = Right "Invalid start symbol"
parseStartSymbol _ _ = Right "Invalid start symbol format"

-- Divide list of strings into strings that we will be passing into rule parsing
parseRules :: [String] -> String -> String -> [RULE] -> Either [RULE] String
parseRules [] _ _ rules = Left rules
parseRules (x:xs) n t rules = case parseRule x n t of
    Left rule -> parseRules xs n t $ rules ++ [rule]
    Right err -> Right err

-- Validate rule, when is valid return RULE and when not return error string
parseRule :: String -> String -> String -> Either RULE  String
parseRule (a:'-':'>':b) n t
    | a `elem` n && not (null b) &&  strConsistOnlyOf b (n ++ t)    = Left $ RULE a b
    | a `elem` n && length b == 1 && strConsistOnlyOf b ['#']       = Left $ RULE a b
    | otherwise                                                     = Right "Invalid symbol in rule"
parseRule _ _ _ = Right "Invalid rule format"

-- Check if list of values is unique
checkUniqueness :: Eq a => [a] -> [a] -> Bool
checkUniqueness [] _ = True
checkUniqueness (x:xs) elems
    | x `elem` elems    = False
    | otherwise         = checkUniqueness xs $ x : elems

-- Check if input string consist only of elements from given string
strConsistOnlyOf :: String -> String -> Bool
strConsistOnlyOf [] _ = True
strConsistOnlyOf (x:xs) elems 
    | x `elem` elems    = strConsistOnlyOf xs elems
    | otherwise         = False
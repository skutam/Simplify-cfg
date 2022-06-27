{-
    Author: Matúš Škuta (xskuta04)
    Project: VUT FIT FLP 1. Project (Functional) - SIMPLIFY-BKG
    Year: 2021
-}

import System.Environment ( getArgs )
import System.IO ( stderr, hPutStrLn, Handle, stdin, openFile, hGetContents, IOMode (ReadMode), hClose )
import System.Exit ( exitFailure )

import Types( BKG(..) )
import ParseInput( parseBKG )
import Minimize ( simplify1, simplify2 )

-- Starting point of program
main :: IO ()
main = do
    -- Parse arguments and receive function to be called
    (func, handle) <- parseArgs =<< getArgs
    -- Get content of given handle
    strings <- hGetContents handle
    -- Parse grammar from STDIN or FILE
    either func exit $ parseBKG $ lines strings
    -- Close handle
    hClose handle

-- Parse input arguments, return action, with handle either from STDIN or FILE
parseArgs :: [String] -> IO (BKG -> IO (), Handle)
parseArgs [] = exit $ "No argument given" ++ help
parseArgs [arg1] = parseAction arg1 stdin
parseArgs [arg1, arg2] = parseAction arg1 =<< openFile arg2 ReadMode
parseArgs _ = exit $ "Invalid arguments" ++ help

-- Based on first argument, return action and given handle
parseAction :: String -> Handle  -> IO (BKG -> IO (), Handle)
parseAction "-i" handle = return (printBKG, handle)
parseAction "-1" handle = return (minimize1PrintBKG, handle)
parseAction "-2" handle = return (minimize2PrintBKG, handle)
parseAction a _ = exit $ "Invalid argument given: " ++ a ++ help

-- Action for argument '-i', where we just load BKG and print it out
printBKG :: BKG -> IO ()
printBKG = putStr . show

-- Action for argument '-1', where we just load BKG, execute algorithm 4.1, and print BKG out
minimize1PrintBKG :: BKG -> IO ()
minimize1PrintBKG bkg = either printBKG printBKG $ simplify1 bkg [] 1

-- Action for argument '-2', where we just load BKG, execute algorithm 4.1, and then algorithm 4.2 when BKG is non empty
minimize2PrintBKG :: BKG -> IO ()
minimize2PrintBKG bkg = either (printBKG . simplify2 [] 0) printBKG $ simplify1 bkg [] 1

-- Print out error message and exit program with error code 1
exit :: String -> IO a
exit str = do
    hPutStrLn stderr str
    exitFailure

-- Return help string of program
help :: String
help = "\nProgram can be run as: ./simplify-bkg (-i|-1|-2) [file]"
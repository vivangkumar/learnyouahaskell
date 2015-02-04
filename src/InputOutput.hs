main = do
    putStrLn "Hello, what's your name?"
    name <- getLine -- Get input
    putStrLn("Hey " ++ name ++ ", you rock!")

import Data.Char

main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your second name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "Hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- without function composition
reverseWords st = unwords (map reverse (words st))

-- getChar
main = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main
        else return ()

-- when
import Control.Monad

main = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main

-- sequence
main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs

-- forever

import Control.Monad
import Data.Char

main = forever $ do
    putStr "Give me some input: "
    line <- getLine
    putStrLn $ map toUpper line

-- forM

import Control.Monad

main = do
    colors <- forM [1, 2, 3, 4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3, 4 are: "
    mapM putStrLn colors

-- getContents

import Data.Char

main = do
    contents <- getContents
    putStr $ map toUpper contents

main = do
    contents <- getContents
    putStr $ shortLinesOnly contents

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter( \line -> length line < 10) allLines
        result = unlines shortLines
    in result

main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines =lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in result

main = interact $ unlines . filter ((<10) . length) . lines

import System.IO 

main = do
    handle <- openFile "test.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

import System.IO

main = do
    withFile "test.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)

-- read file
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile  path mode
    result <- f handle
    hClose handle
    return result

-- write file
import System.IO
import Data.Char

main = do
    contents <- readFile "test.txt"
    writeFile "testwrite.txt" (map toUpper contents)

-- append to file
import System.IO

main = do
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")

-- command line arguments

import System.Environment
import Data.List

main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are:"
    mapM putStrLn args
    putStrLn "The program name is:"
    putStrLn progName


-- Randomness
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

-- Generate random String

import System.Random

main = do
    gen <- getStfGen
    putStr $ take 20 (randomRs ('a', 'z') gen)

import System.Random
import Data.List

main = do
    gen <- getStdGen
    let randomChars = randomRs ('a', 'z') gen
        (first20, rest) = splitAt 20 randomChars
        (second20, _) = splitAt 20 rest
    putStr first20
    putStr second 20

--
import System.Random
import Control.Monad(when)

main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randomNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
    putStr "Which number in the range of 1 to 10 am I thinking of? "
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randNumber == number
            then putStrLn "You are correct!"
            else putStrLn $ "Sorry, it was " ++ sow randomNumber
        askForNumber newGen


---
import System.Environment
import qualified Data.ByteString.Lazy as B

main = do
    (fileName1:fileName2:_) <- getArgs
    copyFile fileName1 fileName2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
    contents <- B.readFile source
    B.writeFile dest contents


-- Exceptions
import System.Environment
import System.IO
import System.Directory

main = do 
    (fileName:_) <- getArgs
    fileExists <- doesFileExist fileName
    if fileExists
        then do contents <- readFile fileName
                putStrLn $ "The file has " ++ show  (length (lines content)) ++ " lines!"
        else do putStrLn "The file does not exist!"

-- With exception handling
import System.Environment
import System.IO
import System.IO.Error

main = toTry `catch` handler

toTry :: IO ()
toTry = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e = putStrLn "Whoops, had some trouble!"

-- or
handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "The file does not exist!"
    | otherwise ioError e

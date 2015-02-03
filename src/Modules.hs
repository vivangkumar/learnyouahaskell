import Data.List

-- Selectively import functions
import Data.List (nub, sort)

-- Import all functions except a few
import Data.List hiding (nub)

-- Qualified import to resolve functions
import import qualified Data.Map as M

numUniques :: (Eq a) => [a] -> Int
nmUniques = length . nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

import Data.Char

filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "Hey guys its me"

-- Cipher
encode :: Int -> String -> String
encode shift message =
    let ords = map ord message
        shifted = map (+ shift) ords
    in map chr shifted

-- alternatively
encode :: Int -> String -> String
encode shift message = map . (chr . (+ shift) . ord) message

decode :: Int -> String -> String
decode shift message = encode (negate shift) message

phoneBook = [("A", "B")
            ,("C", "D")
            ,("E", "F")]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k, v):xs) = if key == k
                             then Just v
                             else findKey key 

-- alternatively using foldl
findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key = foldr(\(k, v) acc -> if key == k then Just v else acc) Nothing

-- Maps
import qualified Data.Map as Map

-- Sets
import import qualified Data.Set as Set
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- Partial function application and function currying
let multTwoWithNine = multThree 9
print multTwoWithNine 2 3

appleTwice :: (a -> a) => a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' = _ [] _ = []
zipWith' = _ _ [] = []
zipWith f(x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [b]
filter _ [] = []
filter p (x:xs) =
    | p x = x: filter p xs
    | otherwise = filter p xs

-- Quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort(filter (>=x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head(filter p [100000, 99999..])
    where p x = x `mod` 3829 == 0

chain :: (Integral a ) => a -> [a]
chain 1 = [a]
chain n
    | even n = n:chain(n `div` 2)
    | odd n = n:chain(n*3 + 1)

-- Lambdas
numLongChain :: Int
numLongChain = length (filter (\xs -> length xs > 15) (map chain[1..100]))


-- Flip with lamdas
flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \(x, y) -> f y x

-- Folds { foldl (\acc) start_value current element}
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- Right folds
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- Foldll and Foldr
maximum' :: (Ord a) => [a] -> a
maximum' = foldrl (\x acc -> x if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldrl (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldrl (\x _ -> x)

last' :: [a] -> a
last' = foldll (\_ x -> x)

-- Function application { Has lowest precedence ($) :: (a -> b) -> a -> b | f $ x = f x }
sum ( map sqrt [1..130]) -- can be written as
sum $ map sqrt [1..130]

-- Function composition
-- f o g(x) = f(g(x))

-- (.) :: (b -> c) -> (a -> b) -> a -> c ? f. g = \x -> f(g x)

map (\x -> negate (abs x)) [5, -3, -6, -7, -8, 2, -14] -- can be written as
map (negate . abs) [] [5, -3, -6, -7, -8, 2, -14]

-- Right associative
map (\xs -> negate (sum (tail xs))) [[1..5], [3..6], [1..7]] -- can be written as
map (negate . sum . tail)

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- Using function composition and application
oddSquareSum :: Integer
oddSquareSum = sum . takewhile (<10000) . filter . odd .map (^2) $ [1..]

oddSquareSum :: Integer
oddSquareSum =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in  sum belowLimit

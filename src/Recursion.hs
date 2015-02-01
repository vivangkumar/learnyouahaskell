-- Maximum function with recursion
max' :: (Ord a) => [a] -> a
max' [] = error "Maximum of empty list!"
max' [x] = x
max' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = max' xs

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- Replicates a number a certain number of times and returns a list
replicate' :: (Num i, Ord a) -> i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n - 1) x

-- Takes a specified number of elements from a list
take' :: (Num i, Ord a) => i -> [a] -> [a]
take' n _
    | n < 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

-- Reverse a list
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- Infinite lists
repeat' :: a -> [a]
repeat' x = x:repeat' x

-- Zip two lists together
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

-- Check if an element is a part of the list
elem' :: (Eq a) -> a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs

-- Quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] =[]
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a >= x]
    in  smallerSorted ++ [x] ++ biggerSorted


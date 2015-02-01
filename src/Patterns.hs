lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial(n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

capital :: String -> String
captial "" = "Empty string."
capital all@(x:xs) = "First letter of "++ all ++" is " ++ [x]

-- Guards

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "Underweight!"
    | bmi <= 25.0 = "You're normal!"
    | bmi <= 30.0 = "You're fat!"
    | otherwise = "You're a whale! Hah!"

bmiTeller :: (RealFloat a) => a -> String
bmiTeller height weight
    | bmi <= 18.5 = "Underweight!"
    | bmi <= 25.0 = "You're normal!"
    | bmi <= 30.0 = "You're fat!"
    | otherwise = "You're a whale! Hah!"
    where bmi = height / weight ^ 2 

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- Defining functions in guards
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

-- Let bindings
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea

-- Inline let
[let square x = x * x in (square 5, suware 6, square 9)]
(let (a, b, b) = (1, 2, 3) in a+b+c)) * 100

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- Case expressions
head' :: [a] -> a
head' xs = case xs of [] -> error 'No head for empty lists!'
                         (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is "++ case xs of [] -> "empty."
                                              [x] -> "a singleton list."
                                              xs -> "a longer list."

describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
    where what [] = "empty."
          what[x] = "a singleton list."
          what xs = "a longer list."
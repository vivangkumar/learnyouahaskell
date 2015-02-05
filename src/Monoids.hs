instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)

main = do
    line <- getLine
    let lineReversed = reverse line
    putStrln $ "Reversed line: " ++ lineReversed

-- Alternatively, using fmap
main = do 
    line <- fmap reverse getLine
    putStrLn $ "Reversed line: " ++ line

instance Functor ((->) r) where
    fmap f g = (\x -> f (g x))

-- Functor laws
-- Mapping the id function over a functor gets us a functor that is the same as the original functor.

fmap id = id -- where id is the original parameter unmodified

instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing

-- Composing two functions and then mapping the resulting function over a functor should be the same as first mapping one function over the functor and then mapping the other one.
fmap (f. g) = fmap f . fmap g

fmap (f .g) F = fmap f (fmap g F)

data CMaybe a = CNothing | Cjust Int a deriving (Show)

instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter + 1) (f x)

-- Applicative Functors
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something

-- Infix fmap
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x

instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)

myAction :: IO String
myAction = do
    a <- getLine
    b <- getLine
    return $ a ++ b

-- Using the Applicative style

myAction :: IO String
myAction = (++) <$> getLine <*> getLine

main = do
    a <- (++) <$> getLine <*> getLine

-- Applicative functor laws
pure f <*> x = fmap f x
pure id <*> v = v
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
pure f <*> pure x = pure (f x)
u <*> pure y = pure ($ y) <*> u

-- newtype
newtype Pair a b = Pair { getPair :: (a, b) }
instance Functor (Pair c) where
    fmap  f (Pair (x, y)) = Pair (f x, y)

-- Monoids
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty

-- Monoid laws
mempty `mappend` x = x
z `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

instance Monoid [x] where
    mempty = []
    mappend = (++)

newtype Product a = Product { getProduct :: a } deriving (Eq, Ord, Read, Show, Bounded)
instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)

-- Any and All
newtype Any = Any { getAny :: Bool } deriving (Eq, Ord, Read, Show, Bounded)
instance Monoid Any where
    empty = Any False
    Any x `mappend` Any y = Any (x || y)

newtype All = All { getAll :: Bool } deriving (Eq, Show, Ord, Read, Bounded)
instance Monoid All where
    mempty = All True
    All x `mappend` All y = All (x && y)

lengthCompare :: String -> String -> Ordering
lengthCompare x y = let a = length x `compare` length y
                        b = x `compare` y
                    in  if a == EQ then b else a

-- using Monoids
import Data.Monoid

instance Monoid Ordering where
    mempty = EQ
    LT `mappend` _ = LT
    EQ `mappend` y = y
    GT `mappend` _ = GT

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend` (x `compare` y)

-- Folding data structures

import import qualified Foldable as F

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

instance F.Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) = F.FoldMap f l `mappend`
                             f x           `mappend`
                             F.Foldmap f r

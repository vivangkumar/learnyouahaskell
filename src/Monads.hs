(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
 -- >>= is called bind

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

class Monad m where
    return :: a -> m a -- similar to pure in Applicative

    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: ma a -> m b -> m b
    x >> y = x >>= \_ -> y

    fail :: String -> m a
    fail msg = error msg

instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = Nothing

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

-- do with monads
foo :: Maybe String
foo = do
    z <- Just 3
    y <- Just "!"
    Just (show x ++ y)

-- list monads

instance Monad [] where
    return x = [x]class
    xs >>= f = concat (map f xs)
    fail _ = []

listOfTuples :: [(Int, Char)]
listOfTuples = do
    n <- [1, 2]
    ch <- ['a', 'b']
    return (n, ch)

class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

instance MonadPlus [] where
    mzero = []
    mplus = (++)

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

sevensOnly :: [Int]
sevensOnly = do
    z <- [1..50]
    guard('7' `elem` show x)
    return x

-- Monad laws

return x >>= f = f x
m >>= return = m
(m >>= f) >>= g = m >>= (\x -> f x >>= g)

-- Writer
newtype Writer a w = Writer { runWriter :: (a, w) }

instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, empty)
    (Writer (x, v)) >>= f =
        let (Writer (y, v')) = f x in WRiter (y, v `mappend` v')

import Control.Monad.Writer
logNumber :: Int -> Writer [String] Int
logNumber x = Writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a * b)

-- State monads

newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in  g newState
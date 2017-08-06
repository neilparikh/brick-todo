module Zipper where

data Zipper a = Zipper {
    left :: [a],
    curr :: a,
    right :: [a]
} deriving Show

instance Functor Zipper where
    fmap f (Zipper l c r) = Zipper (fmap f l) (f c) (fmap f r)

goRight :: Zipper a -> Zipper a
goRight (Zipper l a (x:xs)) = Zipper (a:l) x xs
goRight (Zipper l a []) = Zipper l a []

goLeft :: Zipper a -> Zipper a
goLeft (Zipper (x:xs) a r) = Zipper xs x (a:r)
goLeft (Zipper [] a r) = Zipper [] a r

updateCurrent :: (a -> a) -> Zipper a -> Zipper a
updateCurrent f (Zipper l x r) = Zipper l (f x) r

getCurrent :: Zipper a -> a
getCurrent (Zipper _ x _) = x

toList :: Zipper a -> [a]
toList (Zipper l c r) = (reverse l) ++ (c:r)

fromList :: [a] -> Zipper a
fromList [] = error "empty list cannot be made to zipper"
fromList (x:xs) = Zipper [] x xs

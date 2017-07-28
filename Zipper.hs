module Zipper where

data Zipper a = Zipper [a] a [a] deriving Show

goRight :: Zipper a -> Zipper a
goRight (Zipper left a (x:xs)) = Zipper (a:left) x xs
goRight (Zipper left a []) = Zipper left a []

goLeft :: Zipper a -> Zipper a
goLeft (Zipper (x:xs) a right) = Zipper xs x (a:right)
goLeft (Zipper [] a right) = Zipper [] a right

updateCurrent :: (a -> a) -> Zipper a -> Zipper a
updateCurrent f (Zipper left x right) = Zipper left (f x) right

getCurrent :: Zipper a -> a
getCurrent (Zipper _ x _) = x

toList :: Zipper a -> [a]
toList (Zipper left curr right) = (reverse left) ++ (curr:right)

fromList :: [a] -> Zipper a
fromList [] = error "empty list cannot be made to zipper"
fromList (x:xs) = Zipper [] x xs

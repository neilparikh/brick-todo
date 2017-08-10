module Zipper where
-- TODO: Zipper and NonEmptyZipper share a lot of common code, should find a
-- way to dedup.

data Zipper a = Zipper {
    left :: [a],
    right :: [a]
} deriving Show

instance Functor Zipper where
    fmap f (Zipper l r) = Zipper (fmap f l) (fmap f r)

goRight :: Zipper a -> Zipper a
goRight (Zipper l (x:y:xs)) = Zipper (x:l) (y:xs)
goRight z@(Zipper _ _) = z

goLeft :: Zipper a -> Zipper a
goLeft (Zipper (x:xs) r) = Zipper xs (x:r)
goLeft (Zipper [] r) = Zipper [] r

updateCurrent :: (a -> a) -> Zipper a -> Zipper a
updateCurrent f (Zipper l (x:xs)) = Zipper l ((f x):xs)
updateCurrent _ z@(Zipper _ []) = z

getCurrent :: Zipper a -> Maybe a
getCurrent (Zipper _ (x:xs)) = Just x
getCurrent (Zipper _ []) = Nothing

toList :: Zipper a -> [a]
toList (Zipper l r) = (reverse l) ++ (r)

fromList :: [a] -> Zipper a
fromList [] = Zipper [] []
fromList list = Zipper [] list

offset :: Zipper a -> Int
offset = length . left

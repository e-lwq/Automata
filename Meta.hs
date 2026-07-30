module Meta where

import Data.List ( sort, group )

type State a = a

remDup :: Eq a => [a] -> [a]
remDup [] = []
remDup (x:xs) = x : remDup (filter (/= x) xs)

nodup :: Eq a => [a] -> Bool
nodup [] = True
nodup (x:xs) = notElem x xs && nodup xs

bfsVisited :: Eq a => (a -> [a]) -> [a] -> [a] -> [a]
bfsVisited trans visited [] = visited
bfsVisited trans visited (u:qs) = bfsVisited trans visited' qs'
    where
        neighbours = trans u
        vs' = filter (`notElem` visited) neighbours
        visited' = visited ++ vs'
        qs' = qs ++ vs'

newtype Set a = S [a]
set (S xs) = xs
subset :: Eq a => Set a -> Set a -> Bool
subset (S []) _ = True
subset (S (x:xs)) (S ys) = x `elem` ys && subset (S xs) (S ys)

instance Eq a => Eq (Set a) where
    (==) :: Set a -> Set a -> Bool
    xs == ys = xs `subset` ys && ys `subset` xs

instance Show a => Show (Set a) where
    show (S xs) = show xs


powerSet' :: [a] -> [[a]]
powerSet' [] = [[]]
powerSet' (x : xs) = concatMap f (powerSet' xs)
    where
        f ps = [x : ps, ps]

powerSet :: [a] -> [Set a]
powerSet xs = map S (powerSet' xs)

ts 1 = [2,4]
ts 2 = [4]
ts 3 = [1]
ts 4 = [1]


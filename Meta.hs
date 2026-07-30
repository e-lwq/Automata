module Meta where

import Data.List ( sort, group )

type State a = a

remDup :: Eq a => [a] -> [a]
remDup [] = []
remDup (x:xs) = x : remDup (filter (/= x) xs)

nodup :: Eq a => [a] -> Bool
nodup [] = True
nodup (x:xs) = notElem x xs && nodup xs

powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = concatMap f (powerSet xs)
    where f ps = [x:ps, ps]
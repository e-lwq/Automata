module Meta where

import Data.List ( sort, group )

type State = Int

remDup :: Ord a => [a] -> [a]
remDup xs = map head (group (sort xs))

nodup :: Ord a => [a] -> Bool
nodup xs = all (null . tail) (group (sort xs))
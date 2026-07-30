module NFA_to_DFA where

import Meta
import DFA
import NFA 

convert :: Eq a => NFA a -> DFA [a]
convert (states,alphabets,delta,s,accs) = (states', alphabets, delta', s', accs')
    where
        states' = powerSet states
        delta' rs a = remDup (concatMap (eps delta . flip delta a) rs)
        s' = remDup (eps delta [s])
        accs' = filter (any (`elem` accs)) states'
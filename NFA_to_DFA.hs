module NFA_to_DFA where

import Meta ( remDup, powerSet, Set (S) , set)
import DFA ( DFA )
import NFA ( NFA, genNFA, eps ) 

convert :: Eq a => NFA a -> DFA (Set a)
convert (states,alphabets,delta,s,accs) = (states', alphabets, delta', s', accs')
    where
        states' = powerSet states
        delta' (S rs) a = S (remDup (concatMap (eps delta . flip delta a) rs))
        s' = S (remDup (eps delta [s]))
        accs' = filter (any (`elem` accs) . set) states'

-- Example : NFA of odd # a's to DFA
states = [1,2]
alphabets = ['a','b']
trans = [((1,'a'),[2]),((2,'a'),[1])]
start = 1
accepts = [2]
nfa = genNFA states alphabets trans start accepts

dfa = convert nfa
(states',alphabets',trans',start',accepts') = dfa
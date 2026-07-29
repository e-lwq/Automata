module DFA where

import Data.List ( sort )
import Meta

type Trans = State -> Char -> State

type DFA = ([State],[Char],Trans,State,[State])

type DFAInst = (DFA,State)

initialize :: DFA -> DFAInst
initialize (s,a,d,q,f) = ((s,a,d,q,f),q)

transition :: DFAInst -> Char -> DFAInst
transition ((s,a,d,q,f),state) c = ((s,a,d,q,f),d state c)

process :: DFAInst -> String -> DFAInst
process = foldl transition

accept :: DFA -> String -> Bool
accept dfa ss = let ((s,a,d,q,f),state) = process (initialize dfa) ss
                in state `elem` f

checkValid :: [State] -> [Char] -> [((State,Char),State)] -> State -> [State] -> Bool
checkValid states alphabets edges start accepts = p1 && p2 && p3 && p4
                where
                    p1 = start `elem` states
                    p2 = all (`elem` states) accepts
                    domain = sort (map fst edges)
                    sstates = sort states
                    salphabets = sort alphabets
                    domain' = [(s,c) | s <- sstates, c <- salphabets]
                    p3 = domain == domain'
                    p4 = all ((`elem` states) . snd) edges

genDFA :: [State] -> [Char] -> [((State,Char),State)] -> State -> [State] -> DFA
genDFA states alphabets edges start accepts | cond = (states, alphabets, delta, start, accepts)
        where
            cond = checkValid states alphabets edges start accepts 
            delta s c = (snd . head) (filter ((==(s,c)) . fst) edges)

-- Example: language = {even number of a's}
states = [0,1]
alphabets = ['a','b']
edges = [((0,'a'),1), ((0,'b'),0), ((1,'a'),0), ((1,'b'),1)]
start = 0
accepts = [0]
dfa = genDFA states alphabets edges start accepts
module GNFA where

import Meta
import RegExpr ( RegExpr(A, Empty, Sr), (^), u, sr )
import DFA (DFA, listEdges, genDFA)
import Data.List (group)
import Prelude hiding ((^))

type Trans a = State a -> State a -> RegExpr
-- states does not include qs and qa
type GNFA a = ([State a], [Char], Trans a, State a, State a)

listEdges :: GNFA a -> [((State a, State a), RegExpr)]
listEdges (states,alphabets,trans,qs,qa) = zip domain image
    where
        domain = [(s, t) | s <- qs:states, t <- qa:states]
        image = map (uncurry trans) domain

showEdges :: Show a => [((State a, State a), RegExpr)] -> String
showEdges = foldr cons "\n"
    where
        cons x ss = show x ++ "\n" ++ ss

showGNFA :: (Show a) => GNFA a -> String
showGNFA (states, alphabets, trans, qs, qa) = "States: " ++ show states ++ 
                                            "\nAlphabets: " ++ show alphabets ++ 
                                            "\nEdges: " ++ showEdges (GNFA.listEdges (states,alphabets,trans,qs,qa)) ++ 
                                            "\nStart State: " ++ show qs ++
                                            "\nAccept State: " ++ show qa ++ "\n"

toRegExpr :: [Char] -> RegExpr
toRegExpr [] = Empty
toRegExpr [x] = A x 
toRegExpr (x:xs) = A x `u` toRegExpr xs

convertEdges :: Eq a => [State a] -> [((State a, Char), State a)] -> [((State a, State a), RegExpr)]
convertEdges states edges = zip domain image
    where
        edges' = map (\((s, a), s') -> ((s, s'), a)) edges
        domain = [(s1,s2) | s1 <- states, s2 <- states]
        fetchRes (s1,s2) = map snd (filter ((==(s1,s2)).fst) edges')
        image = map (toRegExpr . fetchRes) domain 

convertDFAtoGNFA :: Eq a => DFA a -> a -> a -> GNFA a
convertDFAtoGNFA (states,alphabets,delta,s,accs) qs qa = (states,alphabets,delta',qs,qa)
    where
        edges = DFA.listEdges states alphabets delta
        edges' = convertEdges states edges
        delta' qi qj | qi==qs && qj==s = A '#'
                    | qi==qs = Empty
                    | qi `elem` accs && qj==qa = A '#'
                    | qj==qa = Empty
                    | otherwise = (snd . head) (filter ((==(qi,qj)).fst) edges')

simplifyGNFA :: GNFA a -> GNFA a
simplifyGNFA ([], alphabets, delta, qs, qa) = ([], alphabets, delta, qs, qa)
simplifyGNFA (s : ss, alphabets, delta, qs, qa) = simplifyGNFA (ss, alphabets, delta', qs, qa)
    where
        delta' qi qj = delta qi qj `u` (delta qi s ^ sr (delta s s) ^ delta s qj)

toGNFA :: (Eq a) => DFA a -> a -> a -> GNFA a
toGNFA dfa qs qa = simplifyGNFA (convertDFAtoGNFA dfa qs qa)

evalDFA :: Ord a => DFA a -> a -> a -> RegExpr
evalDFA dfa qs qa = delta qs qa
    where
        (_,_,delta,_,_) = toGNFA dfa qs qa

-- Example: TB p75 Figure 1.67
states = [1,2]
alphabets = ['a','b']
edges = [((1,'a'),1),((1,'b'),2),((2,'a'),2),((2,'b'),2)]
dfa = genDFA states alphabets edges 1 [2]
expr = evalDFA dfa 3 4

states2 = [1,2,3]
alphabets2 = ['a','b']
edges2 = [((1,'a'),2),((1,'b'),3),((2,'a'),1),((2,'b'),2),((3,'a'),2),((3,'b'),1)]
dfa2 = genDFA states2 alphabets2 edges2 1 [2,3]
expr2 = evalDFA dfa2 4 5
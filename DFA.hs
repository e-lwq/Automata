module DFA where

import Data.List ( sort )
import Meta

type Trans a = State a -> Char -> State a

type DFA a = ([State a],[Char],Trans a,State a,[State a])

type DFAInst a = (DFA a,State a)

listEdges :: [State a] -> [Char] -> Trans a -> [((State a,Char),State a)]
listEdges states alphabets trans = zip domain image
    where
        domain = [(s,a) | s<-states, a<-alphabets]
        image = map (uncurry trans) domain 

showEdges :: Show a => [((State a,Char),State a)] -> String
showEdges = foldr cons "\n"
    where cons x ss = show x ++ "\n" ++ ss

showDFA :: Show a => DFA a -> String
showDFA (states,alphabets,trans,s,accs) = "States: " ++ show states ++
                                        "\nAlphabets: " ++ show alphabets ++
                                        "\nEdges: " ++ showEdges (listEdges states alphabets trans) ++
                                        "\nStart State: " ++ show s ++
                                        "\nAccept States: " ++ show accs ++ "\n"

showDFAInst :: Show a => DFAInst a -> String
showDFAInst (dfa,state) = showDFA dfa ++ "\nCurrent State: " ++ show state ++ "\n"

initialize :: DFA a -> DFAInst a
initialize (s,a,d,q,f) = ((s,a,d,q,f),q)

transition :: DFAInst a -> Char -> DFAInst a
transition ((s,a,d,q,f),state) c = ((s,a,d,q,f),d state c)

process :: DFAInst a -> String -> DFAInst a
process = foldl transition

accept :: Eq a => DFA a -> String -> Bool
accept dfa ss = let ((s,a,d,q,f),state) = process (initialize dfa) ss
                in state `elem` f

checkValid :: Eq a => [State a] -> [Char] -> [((State a,Char),State a)] -> State a -> [State a] -> Bool
checkValid states alphabets edges start accepts = p1 && p2 && p3 && p4 && p5 && p6
                where
                    p1 = start `elem` states
                    p2 = all (`elem` states) accepts
                    domain = map fst edges
                    domain' = [(s,c) | s <- states, c <- alphabets]
                    p3 = S domain == S domain' && nodup domain
                    p4 = all ((`elem` states) . snd) edges
                    p5 = nodup states
                    p6 = nodup alphabets

genDFA :: Eq a => [State a] -> [Char] -> [((State a,Char),State a)] -> State a -> [State a] -> DFA a
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
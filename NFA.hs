module NFA where

import Meta
import Data.List ( sort )

type Trans = State -> Char -> [State] -- '#' for epsilon
type NFA = ([State],[Char],Trans,State,[State])

type NFAInst = (NFA,[State])

initialize :: NFA -> NFAInst
initialize (q,a,t,s,acc) = ((q,a,t,s,acc), eps t [s])

transition :: NFAInst -> Char -> NFAInst
transition ((s,a,d,q,f),states) c = ((s,a,d,q,f),states')
    where
        states' = remDup (concatMap (eps d . flip d c) states)

eps :: Trans -> [State] -> [State]
eps _ [] = []
eps d (st:sts) = st : eps d (d st '#') ++ eps d sts 

process :: NFAInst -> String -> NFAInst
process = foldl transition

showRun :: NFAInst -> String -> [State]
showRun nfa = snd . process nfa

accept :: NFA -> String -> Bool
accept nfa ss = let ((s,a,d,q,f),states) = process (initialize nfa) ss
                in any (`elem` f) states

checkValid :: [State] -> [Char] -> [((State,Char),[State])] -> State -> [State] -> Bool
checkValid states alphabets edges start accepts = p1 && p2 && p3 && p4
                where
                    p1 = start `elem` states
                    p2 = all (`elem` states) accepts
                    domain = map fst edges
                    p (x,y) = x `elem` states && y `elem` ('#':alphabets)
                    p3 = nodup domain && all p domain
                    p4 = all (all (`elem` states) . snd) edges

genNFA :: [State] -> [Char] -> [((State,Char),[State])] -> State -> [State] -> NFA
genNFA states alphabets edges start accepts | cond = (states, alphabets, delta, start, accepts)
        where
            cond = checkValid states alphabets edges start accepts 
            delta s c | null res = []
                    | otherwise = snd (head res)
                where res = filter ((==(s,c)) . fst) edges

-- example: even #a's or odd #b's
states = [1,2,3,4,5]
alphabets = ['a','b']
trans = [((1,'#'),[2,3]),((2,'a'),[4]),((2,'b'),[2]),((4,'a'),[2]),((4,'b'),[4]),((3,'a'),[3]),((3,'b'),[5]),((5,'a'),[5]),((5,'b'),[3])]
start = 1
accepts = [2,5]

nfa = genNFA states alphabets trans start accepts


                        
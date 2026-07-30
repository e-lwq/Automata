module NFA where

import Meta
import Data.List ( sort )

type Trans a = State a -> Char -> [State a] -- '#' for epsilon
type NFA a = ([State a],[Char],Trans a,State a,[State a])

type NFAInst a = (NFA a,[State a])

initialize :: Eq a => NFA a -> NFAInst a
initialize (q,a,t,s,acc) = ((q,a,t,s,acc), remDup (eps t [s]))

transition :: Eq a => NFAInst a -> Char -> NFAInst a
transition ((s,a,d,q,f),states) c = ((s,a,d,q,f),states')
    where
        states' = remDup (concatMap (eps d . flip d c) states)

eps :: Eq a => Trans a -> [State a] -> [State a]
eps d qs = bfsVisited (flip d '#') qs qs

process :: Eq a => NFAInst a -> String -> NFAInst a
process = foldl transition

showRun :: Eq a => NFAInst a -> String -> [State a]
showRun nfa = snd . process nfa

accept :: Eq a => NFA a -> String -> Bool
accept nfa ss = let ((s,a,d,q,f),states) = process (initialize nfa) ss
                in any (`elem` f) states

checkValid :: Eq a => [State a] -> [Char] -> [((State a,Char),[State a])] -> State a -> [State a] -> Bool
checkValid states alphabets edges start accepts = p1 && p2 && p3 && p4 && p5 && p6
                where
                    p1 = start `elem` states
                    p2 = all (`elem` states) accepts
                    domain = map fst edges
                    p (x,y) = x `elem` states && y `elem` ('#':alphabets)
                    p3 = nodup domain && all p domain
                    p4 = all (all (`elem` states) . snd) edges
                    p5 = nodup states
                    p6 = nodup alphabets

genNFA :: Eq a => [State a] -> [Char] -> [((State a,Char),[State a])] -> State a -> [State a] -> NFA a
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


                        
module NFA where

import Meta
import Data.List ( sort )

type Trans a = State a -> Char -> [State a] -- '#' for epsilon
type NFA a = ([State a],[Char],Trans a,State a,[State a])

type NFAInst a = (NFA a,[State a])

listEdges :: [State a] -> [Char] -> Trans a -> [((State a, Char),[State a])]
listEdges states alphabets trans = zip domain image
    where
        domain = [(s,a) | s <- states, a <- '#' : alphabets]
        image = map (uncurry trans) domain

showEdges :: Show a => [((State a, Char), [State a])] -> String
showEdges = foldr cons "\n"
    where cons x ss = show x ++ "\n" ++ ss

showNFA :: Show a => NFA a -> String
showNFA (states, alphabets, trans, s, accs) = "States: " ++ show states ++
                                            "\nAlphabets: " ++ show alphabets ++
                                            "\nEdges: \n" ++ showEdges (listEdges states alphabets trans) ++
                                            "\nStart State: " ++ show s ++
                                            "\nAccept States: " ++ show accs ++ "\n"

showNFAInst :: Show a => NFAInst a -> String
showNFAInst (nfa,states) = showNFA nfa ++ "\nPossible states: " ++ show states ++ "\n"

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

unionNFA :: Eq a => NFA a -> NFA a -> a -> NFA a
unionNFA (ss1, as1, tr1, s1, acc1) (ss2, as2, tr2, s2, acc2) s | cond = (ss,as1,tr,s,acc)
    where
        cond = sort as1 == sort as2 && not (any (`elem` ss2) ss1) && not (any (`elem` ss1) ss2)
        ss = s:ss1++ss2
        tr q a | q == s = if a == '#' then [s1,s2] else []
                | q `elem` ss1 = tr1 q a
                | otherwise = tr2 q a
        acc = acc1 ++ acc2

concatNFA :: Eq a => NFA a -> NFA a -> NFA a
concatNFA (ss1,as1,tr1,s1,acc1) (ss2,as2,tr2,s2,acc2) | cond = (ss,as1,tr,s1,acc2)
    where
        cond = sort as1 == sort as2 && not (any (`elem` ss2) ss1) && not (any (`elem` ss1) ss2)
        ss = ss1 ++ ss2
        tr q a | q `elem` acc1 && a=='#' = s2 : tr1 q a
                | q `elem` ss1 = tr1 q a
                | otherwise = tr2 q a

starNFA :: Eq a => NFA a -> a -> NFA a
starNFA (ss,as,tr,s,acc) s0 = (ss', as, tr', s0, acc')
    where
        ss' = s0 : ss
        tr' q a | q==s0 && a=='#' = [s]
                | q==s0 && a/='#' = []
                | q `elem` acc && a=='#' = s : tr q a
                | otherwise = tr q a
        acc' = s0 : acc

-- example: even #a's or odd #b's
states = [1,2,3,4,5]
alphabets = ['a','b']
trans = [((1,'#'),[2,3]),((2,'a'),[4]),((2,'b'),[2]),((4,'a'),[2]),((4,'b'),[4]),((3,'a'),[3]),((3,'b'),[5]),((5,'a'),[5]),((5,'b'),[3])]
start = 1
accepts = [2,5]

nfa = genNFA states alphabets trans start accepts

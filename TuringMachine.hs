module Turing where

import Meta (nodup, Set (S), subset)

-- '_' for blank symbol
type State a = a
data Dir = L | R
type Transition a = State a -> Char -> (State a, Char, Dir)
type Turing a = ([State a],[Char],[Char],Transition a, State a, State a, State a)

type Config a = (Turing a, ([Char], State a, [Char]))

initialize :: Eq a => Turing a -> [Char] -> Config a
initialize tm input | all (`elem` ls) input = (tm, ([],start,input ++ repeat '_'))
    where
        (_,ls,_,_,start,_,_) = tm

transition :: Eq a => Config a -> Config a
transition (tm, (us,q,v:vs)) | q==acc || q==rej = (tm,(us,q,v:vs))
                            | otherwise = case res of
                                            (q',r,R) -> (tm,(us++[r],q',vs))
                                            (q',r,L) -> if null us then (tm,(us,q',r:vs))
                                                        else (tm,(init us,q',last us:r:vs))
                where
                    res = delta q v
                    (states,letters,tape,delta,start,acc,rej) = tm

accept :: Eq a => Config a -> Bool
accept (tm, (us,q,vs)) | q==acc = True
                        | q==rej = False
                        | otherwise = accept (transition (tm,(us,q,vs)))
        where (_,_,_,_,_,acc,rej) = tm

process :: Eq a => Turing a -> String -> Bool
process tm inp = accept (initialize tm inp)

checkValid :: Eq a => [State a] -> [Char] -> [Char] -> [((State a,Char),(State a,Char,Dir))] -> State a -> State a -> State a -> Bool
checkValid states letters tape edges start acc rej = all id [p1,p2,p3,p4,p5,p6,p7]
        where
            p1 = start `elem` states && acc `elem` states && rej `elem` states
            p2 = acc /= rej
            p3 = S letters `subset` S tape
            p4 = '_' `elem` tape && '_' `notElem` letters
            p5 = nodup states && nodup letters && nodup tape
            domain = map fst edges
            img = map ((\(a,b,c)->(a,b)).snd) edges
            p (x,y) = x `elem` states && y `elem` tape
            p6 = nodup domain && all p domain
            p7 = all p img

genTuring :: Eq a => [State a] -> [Char] -> [Char] -> [((State a,Char),(State a,Char,Dir))] -> State a -> State a -> State a -> Turing a
genTuring states letters tape edges start acc rej | checkValid states letters tape edges start acc rej 
                                                    = (states,letters,tape,delta,start,acc,rej)
                    where
                        delta q a | null res = (rej,a,R)
                                    | otherwise = snd (head res)
                            where
                                res = filter ((==(q,a)) . fst) edges

genEdges :: [[(Char,(Char,Dir,Int))]] -> [((State Int,Char),(State Int,Char,Dir))]
genEdges es = concatMap (uncurry genEdges') (zip [1..] es)

genEdges' :: Int -> [(Char,(Char,Dir,State Int))] -> [((State Int,Char),(State Int,Char,Dir))]
genEdges' _ [] = []
genEdges' s ((a,(b,dir,s')):chs) = ((s,a),(s',b,dir)) : genEdges' s chs

-- Example: Language = {0^(2^n) | n>=0}
states = [1,2,3,4,5,6,7]
letters = ['0']
tape = ['0','_','x']
edges = [[('_',('_',R,7)),('x',('x',R,7)),('0',('_',R,2))],[('x',('x',R,2)),('_',('_',R,6)),('0',('x',R,3))],
        [('x',('x',R,3)),('_',('_',L,5)),('0',('0',R,4))],[('x',('x',R,4)),('_',('_',R,7)),('0',('x',R,3))],
        [('0',('0',L,5)),('x',('x',L,5)),('_',('_',R,2))]]
start = 1
acc = 6
rej = 7
turing = genTuring states letters tape (genEdges edges) start acc rej

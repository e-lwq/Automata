module RegExpr where

import NFA 
import Meta 
import Prelude hiding ((^))

-- '#' for epsilon
data RegExpr = Empty | A Char | Un RegExpr RegExpr | Cc RegExpr RegExpr | Sr RegExpr

bracket :: RegExpr -> String -> String
bracket Empty ss = ss
bracket (A x) ss = ss 
bracket _ ss = "("++ss++")"

instance Show RegExpr where
    show :: RegExpr -> String
    show e = format 0 e 
        where
            format _ Empty = "{}"
            format _ (A x) = [x]
            format p (Un e1 e2) = bracket (p>1) (format 1 e1 ++ "U" ++ format 1 e2)
            format p (Cc e1 e2) = bracket (p>2) (format 2 e1 ++ format 2 e2)
            format _ (Sr e) = format 3 e ++ "*"

            bracket True ss = "("++ss++")"
            bracket False ss = ss

(^) :: RegExpr -> RegExpr -> RegExpr
Empty ^ _ = Empty
_ ^ Empty = Empty
(A '#') ^ x = x 
x ^ (A '#') = x
a ^ b = Cc a b

u :: RegExpr -> RegExpr -> RegExpr
u Empty x = x 
u x Empty = x 
u a b = Un a b

sr :: RegExpr -> RegExpr
sr Empty = A '#'
sr e = Sr e

buildNFA' :: [Char] -> RegExpr -> Int -> (Int, NFA Int)
buildNFA' alphabets Empty i = (i+1, genNFA [i] alphabets [] i [])
buildNFA' alphabets (A c) i = (i+2,genNFA states alphabets edges i [i+1])
    where
        states = [i,i+1]
        edges = [((i,c),[i+1])]
buildNFA' alphabets (Un e1 e2) i = (i''+1, unionNFA nfa1 nfa2 i'')
    where
        (i',nfa1) = buildNFA' alphabets e1 i
        (i'',nfa2) = buildNFA' alphabets e2 i'
buildNFA' alphabets (Cc e1 e2) i = (i'', concatNFA nfa1 nfa2)
    where
        (i',nfa1) = buildNFA' alphabets e1 i
        (i'',nfa2) = buildNFA' alphabets e2 i' 
buildNFA' alphabets (Sr e) i = (i'+1, starNFA nfa i')
    where
        (i',nfa) = buildNFA' alphabets e i

prodAlphabets' :: RegExpr -> [Char]
prodAlphabets' Empty = []
prodAlphabets' (A a) = [a]
prodAlphabets' (Un e1 e2) = prodAlphabets' e1 ++ prodAlphabets' e2
prodAlphabets' (Cc e1 e2) = prodAlphabets' e1 ++ prodAlphabets' e2 
prodAlphabets' (Sr e) = prodAlphabets' e

prodAlphabets :: RegExpr -> [Char]
prodAlphabets expr = remDup (prodAlphabets' expr)

buildNFA :: RegExpr -> NFA Int
buildNFA expr = snd (buildNFA' alphabets expr 0)
    where
        alphabets = prodAlphabets expr

-- Example
e1 = Cc (Sr (A 'a')) (Sr (A 'b')) -- a* . b*
e2 = Sr (A 'a') ^ Sr (A 'b' ^ A 'a' ^ Sr (A 'a')) -- a* . (baa*)*
e3 = Un e1 e2
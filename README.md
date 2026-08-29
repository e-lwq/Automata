# Automata Simulations in Haskell
Definitions of state machines follow that of Michael Sipser's _"Theory of Computation"_.
## Available Functions (listed by file)

### DFA.hs -- Determinstic Finite Automata
`type State a = a`<br>
`type Trans a = State a -> Char -> State a`<br>
`type DFA a = ([State a],[Char],Trans a,State a,[State a])`<br>
`type DFAInst a = (DFA a,State a)`<br>
  - A DFA Instance is a tuple consisting of a DFA, and the current state the automaton is in. 

1. `listEdges :: [State a] -> [Char] -> Trans a -> [((State a,Char),State a)]`<br>
   Convert transition function to the form of a list of graph edges.<br>
   
   1st argument: list of states<br>
   2nd argument: input alphabet<br>
   3rd argument: transition function of DFA<br>
   
2. `showEdges :: Show a => [((State a,Char),State a)] -> String`<br>
   Convert the list of edges to displayable string.<br>
   
   1st argument: list of edges<br>
   
3. `showDFA :: Show a => DFA a -> String`<br>
   Convert DFA to displayable string.<br>
   
   1st argument: DFA<br>
   
4. `showDFAInst :: Show a => DFAInst a -> String`<br>
   Convert instance of DFA to displayable string.<br>
   
   1st argument: DFA Instance<br>
   
5. `process :: DFAInst a -> String -> DFAInst a`<br>
   Process input string using DFA instance and return new DFA instance.<br>
   
   1st argument: DFA instance<br>
   2nd argument: input string<br>
   
6. `accept :: Eq a => DFA a -> String -> Bool`<br>
   Test if DFA accepts input string.<br>
   
   1st argument: DFA<br>
   2nd argument: input string<br>
   
7. `genDFA :: Eq a => [State a] -> [Char] -> [((State a,Char),State a)] -> State a -> [State a] -> DFA a`<br>
   Create DFA with given arguments.<br>
   
   1st argument: list of states<br>
   2nd argument: input alphabet<br>
   3rd argument: list of edges of transition function<br>
   4th argument: start state<br>
   5th argument: list of accept states<br>

## NFA.hs -- Non-deterministic Finite Automata
`'#'` stands for epsilon ie. empty string.

`type Trans a = State a -> Char -> [State a]`<br>
`type NFA a = ([State a],[Char],Trans a,State a,[State a])`<br>
`type NFAInst a = (NFA a,[State a])`<br>
  - An NFA Instance is a tuple consisting of an NFA, and the current state the automaton is in. 

1. `listEdges :: [State a] -> [Char] -> Trans a -> [((State a, Char),[State a])]`<br>
   Convert transition function to the form of a list of graph edges, where each element of this list `((s,c),ls)` means given state `s` and next input character `c`, NFA would transition into states listed in `ls` after reading `c`.
   
   1st argument: list of states<br>
   2nd argument: input alphabet<br>
   3rd argument: transition function of NFA<br>
   
2. `showEdges :: Show a => [((State a, Char), [State a])] -> String`<br>
   Convert the list of edges to displayable string.<br>
   
   1st argument: list of edges<br>
   
3. `showNFA :: Show a => NFA a -> String`<br>
   Convert NFA to displayable string.<br>
   
   1st argument: NFA<br>
   
4. `showNFAInst :: Show a => NFAInst a -> String`<br>
   Convert instance of NFA to displayable string.<br>
   
   1st argument: NFA Instance<br>
   
5. `process :: Eq a => NFAInst a -> String -> NFAInst a`<br>
   Process input string using NFA instance and return new NFA instance.<br>
   
   1st argument: NFA instance<br>
   2nd argument: input string<br>
   
6. `accept :: Eq a => NFA a -> String -> Bool`<br>
   Test if NFA accepts input string.<br>
   
   1st argument: NFA<br>
   2nd argument: input string<br>
   
7. `genNFA :: Eq a => [State a] -> [Char] -> [((State a,Char),[State a])] -> State a -> [State a] -> NFA a`<br>
   Create NFA with given arguments.<br>
   
   1st argument: list of states<br>
   2nd argument: input alphabet<br>
   3rd argument: list of edges of transition function<br>
   4th argument: start state<br>
   5th argument: list of accept states<br>

8. `unionNFA :: Eq a => NFA a -> NFA a -> a -> NFA a`<br>
   Given NFA M and N, return NFA Q which recognizes the union of L(M) and L(N), where L(machine) = language recognized by machine.<br>

   1st argument: NFA M<br>
   2nd argument: NFA N<br>

9. `concatNFA :: Eq a => NFA a -> NFA a -> NFA a`<br>
   Given NFA M and N, return NFA Q which recognizes the concatenation of L(M) and L(N).<br>

   1st argument: NFA M<br>
   2nd argument: NFA N<br>

10. `starNFA :: Eq a => NFA a -> a -> NFA a`<br>
   Given NFA M, return NFA Q which recognizes the star of L(M).<br>
   
   1st argument: NFA M<br>
   
   

   

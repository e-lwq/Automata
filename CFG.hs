module CFG where

type Var = Char
type Terminal = Char
type Rule = (Var,String)
type CFG = ([Var],[Terminal],[Rule],Var)

deriveString :: CFG -> String -> [String]
deriveString cfg [] = []
deriveString (vs,ts,rs,sv) (c:cs) | c `elem` ('#':ts) = ss
                                | otherwise = res ++ ss
                where
                    ss = map (c :) (deriveString (vs, ts, rs, sv) cs)
                    rules = filter ((==c).fst) rs
                    res' = map snd rules
                    res = map (++cs) res'

completed :: CFG -> String -> Bool
completed (_,ts,_,_) = all (`elem` ('#':ts))

deriveStrings :: CFG -> [String] -> ([String], [String])
deriveStrings cfg [] = ([],[])
deriveStrings cfg (s:ss) | completed cfg s = (s:ls,rs)
                        | otherwise = (ls,deriveString cfg s ++ rs)
    where (ls,rs) = deriveStrings cfg ss

derivations' :: CFG -> [String] -> [String]
derivations' cfg [] = []
derivations' cfg ss = done ++ derivations' cfg rem
            where
                (done,rem) = deriveStrings cfg ss

remEps :: String -> String
remEps = filter (/='#')

derivations :: CFG -> [String]
derivations (vs,ts,rs,s) = map remEps (derivations' (vs,ts,rs,s) [[s]])

remEpsRules' :: [Var] -> [(Var,String)] -> [(Var,String)]
remEpsRules' [] rules = rules
remEpsRules' (v:vs) rules = addEpsRules v 

remEpsRules :: [(Var,String)] -> Var -> [(Var,String)]
remEpsRules rules start = remEpsRules' (filter (/=start) epsrs) rules'
    where
        epsrs = map fst (filter ((=="#").snd) rules)
        rules' = filter (\(v,str)-> str/='#' || v==start) rules

toChomsky :: CFG -> Char -> CFG
toChomsky (vs,ts,rs,s) s' = (s':vs,ts,rs''',s')
    where
        rs' = (s',[s]):rs
        rs'' = remEpsRules rs
-- Example
vars = ['S','A','B']
ts = ['a','b']
rs = [('S',"ASA"),('S',"aB"),('A',"B"),('A',"S"),('B',"b"),('B',"#")]
s = 'S'
cnf = (vars,ts,rs,s)
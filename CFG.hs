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

-- Example
vars = ['S','A','B']
ts = ['a','b']
rs = [('S',"ASA"),('S',"aB"),('A',"B"),('A',"S"),('B',"b"),('B',"#")]
s = 'S'
cnf = (vars,ts,rs,s)
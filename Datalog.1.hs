import Data.List

newtype Env =
    Env { env :: [(String, [[String]])]}
    deriving (Show, Eq)

-- Env helper functions
-- The env is a list of tuples - in the first slot is the clause name, and then the second spot is every pair that satisfies that tuple.
envLookupAll :: Env -> String -> [[String]]
envLookupAll (Env []) _ = []
envLookupAll (Env ((x, y):xs)) z = if x == z then y else envLookupAll (Env xs) z

envLookupPrefix :: Env -> String -> [String] -> [[String]]
envLookupPrefix (Env []) _ _ = []
envLookupPrefix env pred xs = filter (\s -> xs `isPrefixOf` s) y
    where y = envLookupAll env pred

envWrite :: Env -> String -> [String] -> Env
envWrite (Env []) key val = Env [(key, [val])]
envWrite (Env ((x, y):xs)) key val = if x == key then (Env ((x, y ++ [val]):xs)) else Env ((x,y):xs')
    where ~(Env xs') = envWrite (Env xs) key val

envRemove :: Env -> String -> [String] -> Env
envRemove (Env []) _ _ = (Env [])
envRemove (Env ((x, y):xs)) pred z = if x == pred then (Env ((x, (envRemoveList y z)):xs)) else Env ((x, y):xs')
  where ~(Env xs') = envRemove (Env xs) pred z

envRemoveList :: [[String]] -> [String] -> [[String]]
envRemoveList [] _ = []
envRemoveList (x:xs) ls = if ls `isPrefixOf` x then xs' else x:xs'
    where xs' = (envRemoveList xs ls)

-- Eval functions
evalConstant :: Env -> Constant -> String
evalConstant env (ConstIdent s) = s
evalConstant env (ConstString s) = s

evalTerm :: Env -> Term -> String
evalTerm env (TermVar s) = s
evalTerm env (TermConst c) = evalConstant env c

evalTerms :: Env -> [Term] -> [String]
evalTerms _ [] = []
evalTerms env (x:xs) = (evalTerm env x):(evalTerms env xs)

evalPredicateSym :: Env -> PredicateSym -> String
evalPredicateSym env (PString s) = s
evalPredicateSym env (PIdent i) = i

evalLit :: Env -> Literal -> (Env, String, [String])
evalLit env (LTerm o term1 term2) = (env, pred, terms)
    where terms = evalTerms env [term1, term2]
          pred = if o == Eq then "=" else "!="
evalLit env (LPred pred ts) = (env, p, terms)
    where terms = evalTerms env ts
          p = evalPredicateSym env pred

evalStatement :: Env -> Statement -> (Env, String)
--evalStatement env (Assertion c) = envWrite env
evalStatement env _ = (env, "")

evalProgram :: Env -> Program -> [(Env, String)]
evalProgram env (Program (x:xs)) = (env', s):(evalProgram env' (Program xs))
    where (env', s) = (evalStatement env x)
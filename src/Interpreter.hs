module Interpreter where

data Variable = Variable{
    name :: String,
    vtype :: String,
    value :: Double 
} deriving Show

type Env = [Variable]

-- Search the value of a variable store in the Env given its name

searchVar :: [Variable] -> String -> [Double]
searchVar [] qName = []
searchVar (x:xs) qName = 
    if name x == qName
        then [value x]
    else searchVar xs qName


readVar :: String -> [Variable] -> c -> [([Variable], Double, c)]
readVar name env input
  = case searchVar env name of
      [] -> []
      [value] -> [(env, value, input)] 
    

modifyEnv :: [Variable] -> Variable -> [Variable]
modifyEnv [] var = [var]
modifyEnv (x:xs) newVar =
    if name x == name newVar
        then newVar: xs
    else x : modifyEnv xs newVar

updateEnv :: Variable -> Env -> c -> [(Env, [Char], c)]
updateEnv var env input
  = case input of { xs -> [(modifyEnv env var, "", xs)] }

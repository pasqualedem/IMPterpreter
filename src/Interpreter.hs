module Interpreter where
import Grammar (AExp (..), BExp (..), Command (..), ComparisonOp (..), AOp (..), BOp (..), Exp (..), Variable (..), Program (..), Type(..), ArrDecl (..))

data Variable = Variable{
    name :: String,
    value :: Type
} deriving Show

type Env = [Interpreter.Variable]

-- Dictionary between grammar arithmetic operations and actual operations
opADict :: Fractional a => AOp -> a -> a -> a
opADict Add = (+)
opADict Sub = (-)
opADict Mul = (*)
opADict Div = (/)

-- Dictionary between grammar comparison operations and actual operations
opCDict :: Ord a => ComparisonOp -> a -> a -> Bool
opCDict Lt = (<)
opCDict Le = (<=)
opCDict Gt = (>)
opCDict Ge = (>=)
opCDict Eq = (==)
opCDict Neq = (/=)

-- Dictionary between grammar boolean operations and actual operations
opBDict :: BOp -> Bool -> Bool -> Bool
opBDict And = (&&)
opBDict Or = (||)

-- Search the value of a variable store in the Env given its name
getVarValue :: Env -> String -> Maybe Type
getVarValue [] _ = Nothing
getVarValue (x:xs) qName = 
    if name x == qName
        then Just (value x)
    else getVarValue xs qName

declareArrayIntensional :: (Ord t, Num t) => t -> [Type]
declareArrayIntensional n
    | n == 0 = []
    | n > 0  =  DoubleType 0: declareArrayIntensional (n-1)

getArrayElement :: (Num t, Ord t) => [Type] -> t -> Type
getArrayElement [] index = error "IndexOutOfRange"
getArrayElement (a:as) index
    | index == 0 = a
    | index > 0 = getArrayElement as (index-1)


setArrayElement :: (Num t, Ord t) => [Type] -> t -> Type -> [Type]
setArrayElement [] index v = error "IndexOutOfRange"
setArrayElement (a:as) index v
    | index == 0 = v:as
    | index > 0 = a:setArrayElement as (index-1) v

readVar :: String -> Env -> c -> [(Env, Type, c)]
readVar name env input
  = case getVarValue env name of
      Nothing -> []
      Just value -> [(env, value, input)] 
    
modifyEnv :: Env -> Interpreter.Variable -> Env
modifyEnv [] var = [var]
modifyEnv (x:xs) newVar =
    if name x == name newVar
        then newVar: xs
    else x : modifyEnv xs newVar

updateEnv :: Interpreter.Variable -> Env -> c -> [(Env, [Char], c)]
updateEnv var env input
  = case input of { xs -> [(modifyEnv env var, "", xs)] }


evalArrayVar :: Env -> Grammar.Variable -> Type
evalArrayVar env (MemLocation v a) = 
  case getVarValue env v of
    Just (ArrayType v) -> 
        case evalAExpr env a of
            Just x -> getArrayElement v x
            Nothing -> error "ArithmeticError"
    Just _ -> error "TypeError"
    Nothing -> error "VariableNotDeclared"


evalVar :: Env -> [Char] -> Grammar.Variable -> Type
evalVar env varType (Identifier v) = 
  case (getVarValue env v, varType) of
    (Just (DoubleType v), "DoubleType") -> DoubleType v
    (Just (BoolType v), "BoolType") -> BoolType v
    (Just _, _) -> error "TypeError"
    (Nothing, _) -> error "VariableNotDeclared"

getDouble :: Type -> Double
getDouble (DoubleType d) = d
getBool :: Type -> Bool
getBool (BoolType b) = b

evalAExpr :: Env -> AExp -> Maybe Double
evalAExpr _ (Number x) = Just x
evalAExpr env (Grammar.AVariable (Identifier v)) = Just (getDouble (evalVar env "DoubleType" (Identifier v)))
evalAExpr env (Grammar.AVariable (MemLocation v a)) = Just (getDouble (evalArrayVar env (MemLocation v a)))
evalAExpr env (AExpOp al op ar) = opADict op <$> evalAExpr env al <*> evalAExpr env ar
 -- evalAExpr env (Negation a) = - <$> (evalAExpr env a) 

evalBExpr :: Env -> BExp -> Maybe Bool
evalBExpr _ (Boolean b) = Just b
evalBExpr env (Grammar.BVariable (Identifier v)) = Just (getBool (evalVar env "BoolType" (Identifier v)))
evalBExpr env (Grammar.BVariable (MemLocation v a)) = Just (getBool (evalArrayVar env (MemLocation v a)))
evalBExpr env (BExpOp bl op br) = opBDict op <$> evalBExpr env bl <*> evalBExpr env br
evalBExpr env (Not b) = not <$> evalBExpr env b
evalBExpr env (Comparison al op ar) = opCDict op <$> evalAExpr env al <*> evalAExpr env ar

execStatement :: Env -> Command -> Env
execStatement env Skip = env
execStatement env (Assignment (Identifier v) (AExp a)) = 
    case evalAExpr env a of
        Just x -> modifyEnv env (Interpreter.Variable v (DoubleType x))
        Nothing -> error "ArithmeticError"
execStatement env (Assignment (MemLocation v av) (AExp a)) = 
    case evalAExpr env a of
        Just x -> modifyEnv env (Interpreter.Variable v (DoubleType x))
        Nothing -> error "ArithmeticError"
execStatement env (ArrayDeclaration (Intensional (Identifier v) a)) =
    case evalAExpr env a of
        Just x -> modifyEnv env (Interpreter.Variable v (ArrayType (declareArrayIntensional x)))
        Nothing -> error "ArithmeticError"
execStatement env (Assignment (Identifier v) (BExp b)) = 
    case evalBExpr env b of
        Just x -> modifyEnv env (Interpreter.Variable v (BoolType x))
        Nothing -> error "BooleanError"
execStatement env (IfThenElse b pthen pelse) =
    case evalBExpr env b of
        Just True -> exec env pthen
        Just False -> exec env pelse
        Nothing -> error "InvalidExpression"
execStatement env (While b p) =
        case evalBExpr env b of
            Just True -> execStatement (exec env p) (While b p)
            Just False -> env
            Nothing -> error "InvalidExpression"

exec :: Env -> Program -> Env
exec env (Single c) = execStatement env c
exec env (Sequence c p) = exec (execStatement env c) p
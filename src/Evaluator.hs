module Evaluator where
import Tree (AExp (..), BExp (..), Command (..), ComparisonOp (..), AOp (..), BOp (..), Exp (..), Variable (..), Program (..), Type(..), ArrDecl (..))

data Variable = Variable{
    name :: String,
    value :: Type
} deriving Show

type Env = [Evaluator.Variable]

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

-- Declare an array given the number of elements
declareArrayIntensional :: Type -> [Type]
declareArrayIntensional (DoubleType d)
    | d == 0 = []
    | d > 0  =  DoubleType 0: declareArrayIntensional (DoubleType (d-1))
    
modifyEnv :: Env -> Evaluator.Variable -> Env
modifyEnv [] var = [var]
modifyEnv (x:xs) newVar =
    if name x == name newVar
        then newVar: xs
    else x : modifyEnv xs newVar

-- Turn the Env into a string
showEnv :: Env -> [Char]
showEnv [] = ""
showEnv ((Variable name value):xs) = show name ++ " = " ++ show value
    ++ "\n" ++ showEnv xs

-- Get an element of an array given an index
getArrayElement :: [p] -> Type -> p
getArrayElement [] (DoubleType index) = error "IndexOutOfRange"
getArrayElement (a:as) (DoubleType index)
    | index == 0 = a
    | index > 0 = getArrayElement as (DoubleType (index-1))

-- Set an element of an array given an index and a value
setArrayElement :: [t] -> Type -> t -> [t]
setArrayElement [] (DoubleType index) v = error "IndexOutOfRange"
setArrayElement (a:as) (DoubleType index) v
    | index == 0 = v:as
    | index > 0 = a:setArrayElement as (DoubleType (index-1)) v

-- Construct an arrat given the number of elements
makeArray :: Env -> [Exp] -> [Type]
makeArray env [exp] = execExpr env exp (:[])
makeArray env (e:ex) = execExpr env e (\x -> x:makeArray env ex)

-- Get a variable from the Env and return its value
evalVar :: Env -> Tree.Variable -> Type
evalVar env (Identifier var) = 
    case getVarValue env var of
        Just (DoubleType v) -> DoubleType v
        Just (BoolType v) -> BoolType v
        Just (ArrayType arr) -> ArrayType arr
        Nothing -> error "VariableNotDeclared"
evalVar env (MemLocation (Identifier m) a) =
    case getVarValue env m of
        Just (ArrayType v) -> execExpr env (AExp a) (getArrayElement v)
        Just _ -> error "TypeError"
        Nothing -> error "VariableNotDeclared"

-- Get a variable value from the Env and check the type matching
safeEvalVar :: Env -> Tree.Variable -> String -> Type
safeEvalVar env var varType = 
    case (evalVar env var, varType) of
        (DoubleType d, "DoubleType") -> DoubleType d
        (BoolType b, "BoolType") -> BoolType b
        (ArrayType a, "ArrayType") -> ArrayType a
        (_, _) -> error "TypeMismatch"     

-- Get unwrapped values
getDouble :: Type -> Double
getDouble (DoubleType d) = d
getBool :: Type -> Bool
getBool (BoolType b) = b

-- Evaluate arithmetic expressions
evalAExpr :: Env -> AExp -> Maybe Double
evalAExpr _ (Number x) = Just x
evalAExpr env (Tree.AVar (Identifier v)) = Just (getDouble (safeEvalVar env (Identifier v) "DoubleType"))
evalAExpr env (Tree.AVar (MemLocation v a)) = Just (getDouble (safeEvalVar env (MemLocation v a) "DoubleType"))
evalAExpr env (AExpOp al op ar) = opADict op <$> evalAExpr env al <*> evalAExpr env ar
evalAExpr env (Negation a) =   (-) <$> Just 0 <*> evalAExpr env a

-- Evaluate boolean expressions
evalBExpr :: Env -> BExp -> Maybe Bool
evalBExpr _ (Boolean b) = Just b
evalBExpr env (Tree.BVar (Identifier v)) = Just (getBool (safeEvalVar env (Identifier v) "BoolType"))
evalBExpr env (Tree.BVar (MemLocation v a)) = Just (getBool (safeEvalVar env (MemLocation v a) "BoolType"))
evalBExpr env (BExpOp bl op br) = opBDict op <$> evalBExpr env bl <*> evalBExpr env br
evalBExpr env (Not b) = not <$> evalBExpr env b
evalBExpr env (Comparison al op ar) = opCDict op <$> evalAExpr env al <*> evalAExpr env ar

-- Evaluate an expression and applicate a function "operation" on it if return Just x
execExpr :: Env -> Exp -> (Type -> p) -> p
execExpr env e operation = 
    case e of
        AExp a ->
            case evalAExpr env a of
                Just x -> operation (DoubleType x)
                Nothing -> error "ArithmeticError"
        BExp b ->
            case evalBExpr env b of
                Just x -> operation (BoolType x)
                Nothing -> error "BooleanError"
        Var v ->
            operation (evalVar env v)

-- Assign a value to an element of an array in the enviroment
arrayElementAssignment :: Env -> String -> AExp -> Type -> Env
arrayElementAssignment env identifier indexExpr value = 
    execExpr env (AExp indexExpr) 
        (\n ->
                case getVarValue env identifier of
                    Just (ArrayType var) -> modifyEnv env (Evaluator.Variable identifier (ArrayType (setArrayElement var n value)))
                    Nothing -> error "ArrayNotDeclared"
        )

-- Execute a statement of the program and apply its effects to the enviroment
execStatement :: Env -> Command -> Env
execStatement env Skip = env
execStatement env (Assignment (Identifier v) (AExp a)) =  execExpr env (AExp a) (modifyEnv env . Evaluator.Variable v)
execStatement env (Assignment (Identifier v) (BExp b)) =  execExpr env (BExp b) (modifyEnv env . Evaluator.Variable v)
execStatement env (Assignment (Identifier v) (Var w)) =  modifyEnv env  (Evaluator.Variable v (evalVar env w))
execStatement env (Assignment (MemLocation (Identifier v) av) (AExp a)) = execExpr env (AExp a) (arrayElementAssignment env v av)
execStatement env (Assignment (MemLocation (Identifier v) av) (BExp b)) = execExpr env (BExp b) (arrayElementAssignment env v av)
execStatement env (Assignment (MemLocation (Identifier v) av) (Var w)) = execExpr env (Var w) (arrayElementAssignment env v av)
execStatement env (ArrayDeclaration (Intensional (Identifier v) a)) = execExpr env (AExp a) (modifyEnv env . Evaluator.Variable v . ArrayType . declareArrayIntensional)
execStatement env (ArrayDeclaration (Extensional (Identifier v) exps)) = modifyEnv env (Evaluator.Variable v (ArrayType (makeArray env exps)))
execStatement env (ArrayDeclaration (Extensional (MemLocation (Identifier v) av) exps)) = arrayElementAssignment env v av  (ArrayType (makeArray env exps))

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

-- Execute a program
exec :: Env -> Program -> Env
exec env (Single c) = execStatement env c
exec env (Sequence c p) = exec (execStatement env c) p
exec env Empty = env
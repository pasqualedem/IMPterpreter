module IMPterpreter.Evaluator where
import IMPterpreter.Tree (AExp (..), BExp (..), Command (..), ComparisonOp (..), AOp (..), BOp (..), Exp (..), Variable (..), Program (..))
import IMPterpreter.Parser (useparse, variable)

data Variable = Variable {
    name :: String,
    value :: VType
} deriving Show

type Env = [IMPterpreter.Evaluator.Variable]

data VType = TDouble Double | TBool Bool | TArray [VType]

instance Show VType where
    show (TDouble d) = show d
    show (TBool d) = show d
    show (TArray d) = show d

exception :: [Char] -> a
exception = errorWithoutStackTrace

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
getVarValue :: Env -> String -> Maybe VType
getVarValue [] _ = Nothing
getVarValue (x:xs) qName = 
    if name x == qName
        then Just (value x)
    else getVarValue xs qName

-- insert or update a new variable  
insertVar :: Env -> IMPterpreter.Evaluator.Variable -> Env
insertVar [] var  = [var] 
insertVar (x:xs) newVar =
    if name x == name newVar
        then newVar: xs
    else x : insertVar xs newVar

-- Turn the Env into a string
showEnv :: Env -> [Char]
showEnv [] = ""
showEnv ((Variable name value):xs) = name ++ " = " ++ show value
    ++ "\n" ++ showEnv xs

-- Set an element of an array given an index and a value
setArrayElement :: VType -> [Double] -> VType -> VType
setArrayElement (TArray []) _ v =  exception "IndexOutOfRange"
setArrayElement (TArray l) [i] v = TArray (f ++ v:as)
    where (f, a:as) = splitAt (floor i) l 
setArrayElement (TArray l) (i:is) v = TArray (f ++ setArrayElement a is v:as)
    where (f, a:as) = splitAt (floor i) l 
setArrayElement _ _ _ = exception "DimensionOutOfBoundsError"

-- Get an element of an array given an index
getArrayElement :: VType -> [Double] -> VType
getArrayElement (TArray []) _ =  exception "IndexOutOfRange"
getArrayElement (TArray l) [i] = l !! floor i
getArrayElement (TArray l) (i:is) = getArrayElement (l !! floor i) is
getArrayElement _ _  = exception "DimensionOutOfBoundsError"

-- Construct an array given the list of expression
makeArrayExtensional :: Env -> [Exp] -> VType
makeArrayExtensional env [exp] = execExpr env exp (\x -> TArray [x])
makeArrayExtensional env (e:ex) = execExpr env e (\x -> TArray (x:t))
    where (TArray t) = makeArrayExtensional env ex

-- Construct an array given the number of elements
makeArrayIntensional :: VType -> VType
makeArrayIntensional (TDouble d)
    | d == 0 = TArray []
    | d > 0  =  TArray (TDouble 0: t)
        where TArray t = makeArrayIntensional (TDouble (d-1))

-- Get a variable from the Env and return its value
evalVar :: Env -> IMPterpreter.Tree.Variable -> VType
evalVar env (Identifier var) = 
    case getVarValue env var of
        Just (TDouble v) -> TDouble v
        Just (TBool v) -> TBool v
        Just (TArray arr) -> TArray arr
        Nothing ->  exception "VariableNotDefined"
evalVar env (ArrayLocation var i) = getArrayElement (evalVar env (Identifier v)) indexes
    where (v, indexes) = unfoldArrayIndexes env (ArrayLocation var i) 

-- Get a variable value from the Env and check the type matching
safeEvalVar :: Env -> IMPterpreter.Tree.Variable -> String -> VType
safeEvalVar env var varType = 
    case (evalVar env var, varType) of
        (TDouble d, "TDouble") -> TDouble d
        (TBool b, "TBool") -> TBool b
        (TArray a, "TArray") -> TArray a
        (_, _) ->  exception "TypeMismatch"     

-- Get unwrapped values
getDouble :: VType -> Double
getDouble (TDouble d) = d
getDouble (TBool b) = exception "TypeMismatch"
getBool :: VType -> Bool
getBool (TBool b) = b
getBool (TDouble d) = exception "TypeMismatch"

-- Evaluate arithmetic expressions
evalAExpr :: Env -> AExp -> Maybe Double
evalAExpr _ (Number x) = Just x
evalAExpr env (AVar (Identifier v)) = Just (getDouble (safeEvalVar env (Identifier v) "TDouble"))
evalAExpr env (AVar (ArrayLocation v a)) = Just (getDouble (safeEvalVar env (ArrayLocation v a) "TDouble"))
evalAExpr env (AExpOp al op ar) = opADict op <$> evalAExpr env al <*> evalAExpr env ar
evalAExpr env (Negation a) = (-) <$> Just 0 <*> evalAExpr env a

-- Evaluate boolean expressions
evalBExpr :: Env -> BExp -> Maybe Bool
evalBExpr _ (Boolean b) = Just b
evalBExpr env (BVar (Identifier v)) = Just (getBool (safeEvalVar env (Identifier v) "TBool"))
evalBExpr env (BVar (ArrayLocation v a)) = Just (getBool (safeEvalVar env (ArrayLocation v a) "TBool"))
evalBExpr env (BExpOp bl op br) = opBDict op <$> evalBExpr env bl <*> evalBExpr env br
evalBExpr env (Not b) = not <$> evalBExpr env b
evalBExpr env (Comparison al op ar) = opCDict op <$> evalAExpr env al <*> evalAExpr env ar


-- Evaluate an expression and applicate a function "operation" on it if return Just x
execExpr :: Env -> Exp -> (VType -> p) -> p
execExpr env e operation = 
    case e of
        AExp a ->
            case evalAExpr env a of
                Just x -> operation (TDouble x)
                Nothing ->  exception "ArithmeticError"
        BExp b ->
            case evalBExpr env b of
                Just x -> operation (TBool x)
                Nothing ->  exception "BooleanError"
        Var v ->
            operation (evalVar env v)
        ArrIntensional n -> operation (execExpr env (AExp n) makeArrayIntensional)
        ArrExtensional exps -> operation (makeArrayExtensional env exps)

-- Assign a value to an element of an array in the enviroment
arrayElementAssignment :: Env -> String -> [Double] -> VType -> Env
arrayElementAssignment env identifier ix value = 
                case getVarValue env identifier of
                    Just (TArray var) -> insertVar env (IMPterpreter.Evaluator.Variable identifier (setArrayElement (TArray var) ix value))
                    _ -> exception "VariableNotArrayError"


unfoldArrayIndexes :: Env -> IMPterpreter.Tree.Variable -> ([Char], [Double])
unfoldArrayIndexes env (ArrayLocation (Identifier id) j) = (id, execExpr env (AExp j) (\x -> [getDouble x]))
unfoldArrayIndexes env (ArrayLocation var j)  = (id, execExpr env (AExp j) (\x -> getDouble x:l))
    where (id, l) = unfoldArrayIndexes env var

-- Execute a statement of the program and apply its effects to the enviroment
execStatement :: Env -> Command -> Env
execStatement env Skip = env
execStatement env (Assignment (Identifier v) e) =  execExpr env e (insertVar env . IMPterpreter.Evaluator.Variable v)
execStatement env (Assignment (ArrayLocation var i) e) = execExpr env e (arrayElementAssignment env id idx)
    where (id, idx) = unfoldArrayIndexes env (ArrayLocation var i)

execStatement env (IfThenElse b pthen pelse) = execExpr env (BExp b) 
    (\bvalue -> if getBool bvalue then exec env pthen else exec env pelse)
execStatement env (While b p) = execExpr env (BExp b) 
    (\bvalue -> if getBool bvalue then exec (exec env p) (Single (While b p)) else env)

-- Execute a program
exec :: Env -> Program -> Env
exec env (Single c) = execStatement env c
exec env (Sequence c p) = exec (execStatement env c) p
exec env Empty = env
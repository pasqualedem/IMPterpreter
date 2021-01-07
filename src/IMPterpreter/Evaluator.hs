module IMPterpreter.Evaluator where
import IMPterpreter.Tree (AExp (..), BExp (..), Command (..), ComparisonOp (..), AOp (..), BOp (..), Exp (..), Variable (..), Program (..))
import IMPterpreter.Parser (variable)
import IMPterpreter.Exception (Exception (..))

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

-- Check if an expression raises an exception and propagates it
checkException :: Either t b -> (t -> a) -> Either a b
checkException toCheck operation = 
    case toCheck of
        Left value -> Left (operation value)
        Right s -> Right s

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
getVarValue :: Env -> String -> Either VType Exception 
getVarValue [] v = Right (VariableNotDefined (show v))
getVarValue (x:xs) qName = 
    if name x == qName
        then Left (value x)
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
setArrayElement :: VType -> [Double] -> VType -> Either VType Exception 
setArrayElement (TArray []) i v =  Right (IndexOutOfBounds (floor <$> i))
setArrayElement (TArray l) [i] v = 
    case splitAt (floor i) l of
        (f, a:as) -> Left (TArray (f ++ (v:as)))
        (f, []) -> Right (IndexOutOfBounds [floor i])
setArrayElement (TArray l) (i:is) v = 
    case setArrayElement a is v of
        Left (TArray arr) -> Left (TArray (f ++ [TArray arr] ++ as))
        Right s -> Right s
    where (f, a:as) = splitAt (floor i) l 
setArrayElement _ i _ = Right (DimensionOutOfBounds (floor <$> i))


-- setArrayElement :: VType -> [Double] -> VType -> VType
-- setArrayElement (TArray []) _ v =  exception "IndexOutOfRange"
-- setArrayElement (TArray l) [i] v = TArray (f ++ v:as)
--     where (f, a:as) = splitAt (floor i) l 
-- setArrayElement (TArray l) (i:is) v = TArray (f ++ setArrayElement a is v:as)
--     where (f, a:as) = splitAt (floor i) l 

safeArrayAccess :: [a] -> Int -> Either a Exception
safeArrayAccess l i = 
    if length l > i && i >= 0 then Left (l !! i) else Right (IndexOutOfBounds [i])

-- Get an element of an array given an index
getArrayElement :: VType -> [Double] -> Either VType Exception 
getArrayElement (TArray l) [i] = safeArrayAccess l (floor i)
getArrayElement (TArray l) (i:is) = getArrayElement (l !! floor i) is
getArrayElement _ idx  = Right (DimensionOutOfBounds (floor <$> idx))

-- Construct an array given the list of expression
makeArrayExtensional :: Env -> [Exp] -> Either VType Exception
makeArrayExtensional env [exp] = execExpr env exp (\x -> Left (TArray [x]))
makeArrayExtensional env (e:ex) = execExpr env e (\x -> Left (TArray (x:t)))
    where Left (TArray t) = makeArrayExtensional env ex

-- Construct an array given the number of elements
makeArrayIntensional :: VType -> VType
makeArrayIntensional (TDouble d)
    | d <= 0 = TArray []
    | d > 0  =  TArray (TDouble 0: t)
        where TArray t = makeArrayIntensional (TDouble (d-1))

-- Get a variable from the Env and return its value
evalVar :: Env -> IMPterpreter.Tree.Variable -> Either VType Exception
evalVar env (Identifier var) = getVarValue env var
evalVar env (ArrayLocation var i) = 
    case unfoldArrayIndexes env (ArrayLocation var i) of
        (v, Left indexes) ->
            case evalVar env (Identifier v) of 
                Left value -> getArrayElement value indexes
                Right err -> Right err
        (v, Right err) -> Right err

-- Get a variable value from the Env and check the type matching
safeEvalVar :: Env -> IMPterpreter.Tree.Variable -> [Char] -> Either VType Exception 
safeEvalVar env var varType = 
    case (evalVar env var, varType) of
        (Left (TDouble d), "TDouble") -> Left(TDouble d)
        (Left (TBool b), "TBool") -> Left (TBool b)
        (Left (TArray a), "TArray") -> Left (TArray a)
        (Right s, _) ->  Right s  
        (Left v, t) -> Right (TypeMismatchValue (show t) (show v))

-- Get unwrapped values
getDouble :: Either VType Exception  -> Either Double Exception 
getDouble (Left (TDouble d)) = Left d
getDouble (Left (TBool b)) = Right (TypeMismatchType "TDouble" "TBool")
getDouble (Right s) = Right s
getBool :: Either VType Exception -> Either Bool Exception
getBool (Left (TBool b)) = Left b
getBool (Left (TDouble d)) = Right (TypeMismatchType "TBool" "TDouble")
getBool (Right s) = Right s

-- Evaluate arithmetic expressions

evalAExpr :: Env -> AExp -> Either Double Exception 
evalAExpr _ (Number x) = Left x
evalAExpr env (AVar (Identifier v)) = getDouble (safeEvalVar env (Identifier v) "TDouble")
evalAExpr env (AVar (ArrayLocation v a)) = getDouble (safeEvalVar env (ArrayLocation v a) "TDouble")
evalAExpr env (AExpOp al op ar) = 
    case (evalAExpr env al, evalAExpr env ar) of
        (Left l, Left r) -> Left (opADict op l r)
        (_, Right s) -> Right s
        (Right s, _) -> Right s
evalAExpr env (Negation a) = checkException (evalAExpr env a) (\x -> -x)

-- Evaluate boolean expressions
evalBExpr :: Env -> BExp -> Either Bool Exception
evalBExpr _ (Boolean b) = Left b
evalBExpr env (BVar (Identifier v)) = getBool (safeEvalVar env (Identifier v) "TBool")
evalBExpr env (BVar (ArrayLocation v a)) = getBool (safeEvalVar env (ArrayLocation v a) "TBool")
evalBExpr env (BExpOp bl op br) =
    case (evalBExpr env bl, evalBExpr env br) of
        (Left l, Left r) -> Left (opBDict op l r)
        (_, Right s) -> Right s
        (Right s, _) -> Right s
evalBExpr env (Not b) = checkException (evalBExpr env b) not
evalBExpr env (Comparison al op ar) = 
    case (evalAExpr env al, evalAExpr env ar) of
        (Left l, Left r) -> Left (opCDict op l r)
        (_, Right s) -> Right s
        (Right s, _) -> Right s

-- Evaluate an expression and applicate a function "operation" on it if return Just x
execExpr :: Env -> Exp -> (VType -> Either a Exception) -> Either a Exception 
execExpr env e operation = 
    case e of
        AExp a ->
            case evalAExpr env a of
                Left x -> operation (TDouble x)
                Right s ->  Right s
        BExp b ->
            case evalBExpr env b of
                Left x -> operation (TBool x)
                Right s ->  Right s
        Var v ->
            case evalVar env v of
                Left x -> operation x
                Right s ->  Right s
        ArrIntensional n -> 
            case execExpr env (AExp n) (Left . makeArrayIntensional) of
                Left x -> operation x
                Right s ->  Right s
        ArrExtensional exps -> 
            case makeArrayExtensional env exps of
                Left arr -> operation arr
                Right s ->  Right s

-- Assign a value to an element of an array in the enviroment
arrayElementAssignment :: Env -> String -> [Double] -> VType -> Either Env Exception
arrayElementAssignment env identifier ix value = 
                case getVarValue env identifier of
                    Left (TArray var) -> 
                        checkException (setArrayElement (TArray var) ix value) (insertVar env . IMPterpreter.Evaluator.Variable identifier)
                    Left t -> Right (TypeMismatchValue "TArray" (show t))
                    Right s -> Right s

unfoldArrayIndexes :: Env -> IMPterpreter.Tree.Variable -> ([Char], Either [Double] Exception)
unfoldArrayIndexes env (ArrayLocation (Identifier id) j) = (id, execExpr env (AExp j) 
    (\x -> 
        checkException (getDouble (Left x)) (:[])
            ))
unfoldArrayIndexes env (ArrayLocation var j)  = (id, execExpr env (AExp j) (\x -> case getDouble (Left x) of Left d -> Left (d:l); Right s -> Right s))
    where (id, Left l) = unfoldArrayIndexes env var

-- Execute a statement of the program and apply its effects to the enviroment
execStatement :: Env -> Command -> Either Env Exception 
execStatement env Skip = Left env
execStatement env (Assignment (Identifier v) e) =  execExpr env e (Left . insertVar env . IMPterpreter.Evaluator.Variable v)
execStatement env (Assignment (ArrayLocation var i) e) = execExpr env e (arrayElementAssignment env id idx)
    where (id, Left idx) = unfoldArrayIndexes env (ArrayLocation var i)

execStatement env (IfThenElse b pthen pelse) = execExpr env (BExp b) 
    (\bvalue ->
        case getBool (Left bvalue) of
            Left True -> exec env pthen
            Left False -> exec env pelse
            Right s -> Right s
    )
execStatement env (While b p) = execExpr env (BExp b) 
    (\bvalue ->
        case getBool (Left bvalue) of
            Left True -> 
                case exec env p of
                    Left e -> exec e (Single (While b p))
                    Right s -> Right s
            Left False -> Left env
            Right s -> Right s
    )

-- -- Execute a program
exec :: Env -> Program -> Either Env Exception 
exec env (Single c) = execStatement env c
exec env (Sequence c p) =
    case execStatement env c of
        Left e -> exec e p
        Right s -> Right s
exec env Empty = Left env
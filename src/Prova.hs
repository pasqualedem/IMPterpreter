import IMPterpreter.Parser
import IMPterpreter.Evaluator
import IMPterpreter.Tree


expr = ArrayLocation (ArrayLocation (Identifier "A") (Number 0.0)) (Number 0.0)
Left env =  exec [] (fst (parse "A = [[1, 2], [3, 4]];"))

idx = unfoldArrayIndexes env expr

Left v = getVarValue env "A"
TArray arr = v
i = [0, 0]
Left v1  = setArrayElement v i (TDouble 34)

(f, a:as) = splitAt (floor 0) arr

Left (TArray scase) = setArrayElement a [0] (TDouble 3)
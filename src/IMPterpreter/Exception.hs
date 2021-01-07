module IMPterpreter.Exception where

data Exception
    = VariableNotDefined String 
    | IndexOutOfBounds [Int]
    | TypeMismatchType String String
    | TypeMismatchValue String String
    | DimensionOutOfBounds [Int]

instance Show Exception where
    show (VariableNotDefined s) = "Error: VariableNotDefined " ++ s
    show (IndexOutOfBounds i) = "Error: IndexOutOfBounds " ++ show i
    show (DimensionOutOfBounds i) = "Error: DimensionOutOfBounds " ++ show i
    show (TypeMismatchType t v) = "Error: TypeMismatch, expected" ++ t ++ ", got " ++ v
    show (TypeMismatchValue t1 t2) = "Error: TypeMismatch, expected" ++ t1 ++ ", got " ++ t2
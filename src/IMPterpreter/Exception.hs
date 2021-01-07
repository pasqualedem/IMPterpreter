module IMPterpreter.Exception where

data Exception
    = VariableNotDefined String 
    | IndexOutOfBounds [Int]
    | TypeMismatchType String String
    | TypeMismatchValue String String
    | DimensionOutOfBounds [Int]

instance Show Exception where
    show (VariableNotDefined s) = "VariableNotDefined " ++ s
    show (IndexOutOfBounds i) = "IndexOutOfBounds " ++ show i
    show (DimensionOutOfBounds i) = "DimensionOutOfBounds " ++ show i
    show (TypeMismatchType t v) = "TypeMismatch, expected" ++ t ++ ", got " ++ v
    show (TypeMismatchValue t1 t2) = "TypeMismatch, expected" ++ t1 ++ ", got " ++ t2
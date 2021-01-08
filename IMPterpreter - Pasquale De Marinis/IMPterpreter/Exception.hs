module IMPterpreter.Exception where

data Exception
    = VariableNotDefined String 
    | IndexOutOfBounds [Int]
    | TypeMismatchType String String
    | TypeMismatchValue String String
    | DimensionOutOfBounds [Int]

instance Show Exception where
    show (VariableNotDefined s) = "Exception: VariableNotDefined " ++ s
    show (IndexOutOfBounds i) = "Exception: IndexOutOfBounds " ++ show i
    show (DimensionOutOfBounds i) = "Exception: DimensionOutOfBounds " ++ show i
    show (TypeMismatchValue t v) = "Exception: TypeMismatch, expected " ++ t ++ " value, got " ++ v
    show (TypeMismatchType t1 t2) = "Exception: TypeMismatch, expected " ++ t1 ++ ", got " ++ t2
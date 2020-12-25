module Grammar where

data Program
    = Single Command
    | Empty
    | Program Command Program
    deriving Show

data Command
    = Skip
    | Assignment Variable Exp
    | IfThenElse BExp Program Program
    | While BExp Program
    deriving Show

data Exp
    = AExp AExp
    | BExp BExp
    deriving Show

data AExp
    = Constant Double
    | AExpOp AExp AOp AExp
    | ATermOp ATerm AOp AExp
    | Negation Minus AExp
    | ATerm ATerm
    deriving Show

data ATerm
    = Number Double
    | AVariable Variable
    deriving Show
 
data AOp
    = Add
    | Sub
    | Mul
    | Div
    deriving Show

data BExp
    = BTermOp BTerm BOp BExp
    | BExpOp BExp BOp BExp
    | Inversion Not BExp
    | BTerm BTerm
    deriving Show

data BTerm
    = ABExp AExp
    | Comparison AExp ComparisonOp AExp
    | BVariable Variable
    | Boolean Bool
    deriving Show

data BOp
    = Or
    | And
    deriving Show

data ComparisonOp
  = Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Neq
  deriving Show

data Minus = Minus
    deriving Show

data Not = Not
    deriving Show

newtype Variable = AlphaVar String
    deriving Show

--main = Not (Comparison (Negation Minus (AExpOp (Negation Minus (Constant 3)) Add (Constant 3))) Lt (Constant 3))
b = AExpOp (AExpOp (Constant 3) Sub (Constant 5)) Add (Constant 3)
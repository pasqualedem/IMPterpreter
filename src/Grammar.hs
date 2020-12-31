module Grammar where

data Type = DoubleType Double | BoolType Bool | ArrayType [Type]
  deriving Show

data Program
    = Empty
    | Single Command
    | Sequence Command Program
    deriving Show

data Command
    = Skip
    | ArrayDeclaration ArrDecl
    | Assignment Variable Exp
    | IfThenElse BExp Program Program
    | While BExp Program
    deriving Show

data ArrDecl
    = Intensional Variable AExp
    | Extensional [Exp]
    deriving Show

data Exp
    = AExp AExp
    | BExp BExp
    deriving Show

data AExp
    = Number Double
    | AVariable Variable
    | AExpOp AExp AOp AExp
    | Negation AExp
    deriving Show

data AOp
    = Add
    | Sub
    | Mul
    | Div
    deriving Show

data BExp
    = BExpOp BExp BOp BExp
    | Not BExp
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

data Variable  
    = Identifier [Char]
    | MemLocation [Char] AExp
    deriving Show

type Constructor = [Char]

--main = Not (Comparison (Negation Minus (AExpOp (Negation Minus (Constant 3)) Add (Constant 3))) Lt (Constant 3))
-- b = AExpOp (AExpOp (Constant 3) Sub (Constant 5)) Add (Constant 3)
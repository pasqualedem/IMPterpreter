module Tree where

data Type = DoubleType Double | BoolType Bool | ArrayType [Type]

instance Show Type where
    show (DoubleType d) = show d
    show (BoolType d) = show d
    show (ArrayType d) = show d

data Program
    = Empty
    | Single Command
    | Sequence Command Program
    deriving (Show, Eq)

data Command
    = Skip
    | ArrayDeclaration ArrDecl
    | Assignment Variable Exp
    | IfThenElse BExp Program Program
    | While BExp Program
    deriving (Show, Eq)

data ArrDecl
    = Intensional Variable AExp
    | Extensional Variable [Exp]
    deriving (Show, Eq)

data Exp
    = Var Variable
    | BExp BExp
    | AExp AExp
    deriving (Show, Eq)

data AExp
    = Number Double
    | AVar Variable
    | AExpOp AExp AOp AExp
    | Negation AExp
    deriving (Show, Eq)

data AOp
    = Add
    | Sub
    | Mul
    | Div
    deriving (Show, Eq)

data BExp
    = BExpOp BExp BOp BExp
    | Not BExp
    | Comparison AExp ComparisonOp AExp
    | BVar Variable
    | Boolean Bool
    deriving (Show, Eq)

data BOp
    = Or
    | And
    deriving (Show, Eq)

data ComparisonOp
  = Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Neq
  deriving (Show, Eq)

data Variable  
    = Identifier [Char]
    | MemLocation Variable AExp
    deriving (Show, Eq)

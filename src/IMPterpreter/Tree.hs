module IMPterpreter.Tree where

type Program = [Command]

data Command
    = Skip
    | Assignment Variable Exp
    | IfThenElse BExp Program Program
    | While BExp Program
    deriving (Show, Eq)


data Exp
    = Var Variable
    | BExp BExp
    | AExp AExp
    | ArrIntensional AExp
    | ArrExtensional [Exp]
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
    = Boolean Bool 
    | Not BExp
    | Comparison AExp ComparisonOp AExp
    | BVar Variable
    | BExpOp BExp BOp BExp
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
    | ArrayLocation Variable AExp
    deriving (Show, Eq)
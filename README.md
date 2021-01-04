# IMPterpreter



## 1. Introduction

An interpreter is a program that directly analyse and execute instructions written in a certain programming language. In this project was built a simple interpreter in Haskell based on modified strategy given in the book *Programming in Haskell – Second Edition" by Graham Hutton*. IMPterpreter works on a simple imperative programming language called IMP.

The basic constructs of IMP are:

 - Skip: does nothing
 - Assignment: evaluate an rvalue and assign it to a variable
 - If then else: Performs the conditional selection between two paths
 - While: Loop a set of instructions according to a boolean condition

IMPterpreter uses an eager evaluation (call-by-value), so in functions calls as g(f(x)) is evaluate first g and subsequently f. 

## 2. Grammar
W.t.r. to the original grammar of IMP, it has been modified to allow the representation of real numbers and arrays. The produced grammar is the following:
```EBNF
Program := <Command> | <Command> <Program>
Command ::= <Skip> | <ArrayDeclaration> | <Assignment> | <IfThenElse> | <While> 
Skip ::= "skip" <Semicolon> 
Assignment ::= <Variable> ":=" <Exp> <Semicolon>
IfThenElse ::= "if" <Space> <bexp> <Space> "then" <Space> (<Program> | <Program> "else" <Space> <Program>) "end" <Semicolon>
While ::= "while" <space> <BExp> <Space> "do" <Space> <Program> "end" <Semicolon> 
Exp ::= AExp | BExp | ( "Array" "(" <AExp> ")" | "[" <Exp> ["," <Exp> ]* "]"  )

BExp ::= <BTerm> | <BTerm> "||" <BExp>
BTerm ::= <BFact> | <BTerm> "&&" <BFact>
BFact ::= <AExp> <ComparisonOp> <AExp> | <Bool> | <Variable> | "!" <BExp> | "(" <bexp> ")"

AExp ::= <ATerm> | <ATerm> "+" <AExp> | <ATerm> "-" <AExp>
ATerm = <AFactor> | <AFactor> "*" <ATerm> | <AFactor> "/" <ATerm>
AFactor = <PositiveNumber> | <Variable> | "(" <AExp> ")" | "-" <AExp>
PositiveNumber ::= <Digit> <PositiveNumber> | <PositiveNumber> "." <PositiveNumber> | <Digit>
Digit ::= [0-9]

Identifier ::= [<Letter>]{<Letter> | <Letter>}
Variable ::= Identifier "[" AExp "]"
ComparisonOp ::= "<" | ">" | "=" | "<=" | ">=" | "!=" 
Letter ::= [a-z]
Semicolon ::= ";" | ";" <space> 
Spacee ::= " "  
```

## 3. Architecture and implementation

The interpreter is made out by two modules:
 - **The parser** : Given an input program processes it returning a tree representation of the program
 - **The evaluator** : Given the output of the parser and an initial environment, it executes the program producing a final environment 


### 3.1 Parser

The parser can be seen as a function that given a String, produces a representation of the program in a tree structure, in this is case we see the tree structure as a generic type "a", moreover a part of the string or the entire can be not consumed sometimes, so it can return the unconsumed part and can return Nothing if the parsing completely fails; so the definition is: 

```Haskell
newtype Parser a = P (String -> Maybe (a, String))
```
#### 3.1.1 Tree representation

The internal representation of the program is made with a hierarchy of haskell data constructors.

Starting from the top, a program is an **Empty** program, a **Single Command**, or a **Sequence** **Command Program**. Single Command is redundant but allow us to have a more compact structure. 

```haskell
data Program
    = Empty
    | Single Command
    | Sequence Command Program
    deriving (Show, Eq)
```

A Command is one of the basic construct of IMP. Differently from the grammar, the **ArrayDeclaration** is separated from the **Assignment** in order to have more modularity, and a simple structure.

```haskell
data Command
    = Skip
    | ArrayDeclaration ArrDecl
    | Assignment Variable Exp
    | IfThenElse BExp Program Program
    | While BExp Program
    deriving (Show, Eq)
```

The array declaration can be **Intensional** giving the number of elements, of **extentional**, giving the sequence of expressions.

```haskell
data ArrDecl
    = Intensional Variable AExp
    | Extensional Variable [Exp]
    deriving (Show, Eq)
```

An expression can be an **arithmetic** or **boolean** expression, or a single **Variable**. The single variable case is necessary because in that case we can not infer if it is boolean or arithmetic.

```haskell
data Exp
    = Var Variable
    | BExp BExp
    | AExp AExp
    deriving (Show, Eq)
```

An arithmetic expression can be a **Double**, a **Variable**, an **operation** *(Add, Sub, Mul, Div)* between two expressions, or a **negated** expression.

```haskell
data AExp
    = Number Double
    | AVar Variable
    | AExpOp AExp AOp AExp
    | Negation AExp
    deriving (Show, Eq)
```

A boolean expression can be a **Bool**, a **negated** expression, a **comparison** *(<, <=, >, >=, ==, !=)* between two arithmetic expressions, a **Variable**, and an **operation**  *(And, Or)* between two boolean expressions.

```haskell
data BExp
    = Boolean Bool 
    | Not BExp
    | Comparison AExp ComparisonOp AExp
    | BVar Variable
    | BExpOp BExp BOp BExp
    deriving (Show, Eq)
```

A Variable is an **identifier** or an **ArrayLocation**, so a Variable with an index.

```haskell
data Variable  
    = Identifier [Char]
    | ArrayLocation Variable AExp
    deriving (Show, Eq)
```

#### 3.1.2 Functor, Applicative, Monad, Alternative

The following four instances are the core of the parser, because allow multiple parsers to operate in parallel and in sequence.

**Functor**

The Functor class provides the function *fmap*, in order to give the possibility to apply a function g to a wrapped value. In our implementation, if the result of out parsing is `Nothing`, it gives `Nothing`, otherwise it returns a result (v, out), we apply g to v and then wrap it in `Just`. In any case we wrap the whole in the Parser type.

```Haskell
instance Functor Parser where
    fmap g (P p) =
        P
        ( \input -> case p input of
            Nothing -> Nothing
            Just (v, out) -> Just (g v, out)
        )
```
**Applicative**

The Applicative class provides the function `pure` that simply wraps the given input, and the function `<*>`  used to apply a wrapped function to a wrapped value. In our case we extract the wrapped function `pg` and apply an input to it, if it gives Nothing, then Nothing is returned, otherwise we use the fmap function to the result g and the other wrapped value `px`. The result `(P p)` is applied to out. 

```Haskell
instance Applicative Parser where
    pure v = P (\input -> Just (v, input))
    (P pg) <*> px =
        P
        ( \input -> case pg input of
            Nothing -> Nothing
            Just (g, out) -> case fmap g px of
                (P p) -> p out
        )
```

**Monad**

The Monad class provides the function *bind* (>>=). It takes a wrapped value `m a`, a function `a -> m b` and returns  `m b`.  The followed path is similar to Applicative and Functor, if our parsing fails, giving Nothing, the failure is propagated, otherwise we apply the function on the result `v` and if it returns `(P p)` we apply to out. 

Given an instance to Monad, we can use the `do` notation to combine parsers in sequence.

```haskell
instance Monad Parser where
    (P p) >>= f =
        P
        ( \input -> case p input of
            Nothing -> Nothing
            Just (v, out) -> case f v of
                (P p) -> p out
        )
```

**Alternative**

As the name says, Alternative can be used to choose between multiple alternatives or to allow parallel computing. In this case given two Parsers p and q, if the first fails, we use q, otherwise we return the results of p.  `empty` represents an applicative computation with zero results, in this case we return the failure `Nothing`.


```Haskell
instance Alternative Parser where
    empty = P (const Nothing)
    (P p) <|> (P q) =
        P
        ( \input -> case p input of
            Nothing -> q input
            Just (v, out) -> Just (v, out)
        )
```

Also, with alternative we can use the function `many`, that applies the parser many times as possible, and `some`, that has the difference that at least one parser have to succeeds.

```haskell
class Monad f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
    many :: f a -> f [a]
    some :: f a -> f [a]
    many x = some x <|> pure []
    some x = ((:) <$> x) <*> many x
```

#### 3.1.3 Token parsing

in order to parse the tokens of the language we need to start from parsing a single char, thus the Parser `item` do this, it fails with the empty string otherwise consumes the first character.

```haskell
item :: Parser Char
item =
  P
    ( \input -> case input of
        [] -> Nothing
        (x : xs) -> Just (x, xs)
    )
```

 The other basic element is `sat`, it succeeds if the given char satisfies the predicate `p` .

```haskell
sat :: (Char -> Bool) -> Parser Char
sat p = 
    do
        x <- item;
        if p x then return x else empty;
```

Now is possible to define the following parsers:

- digit: parses a digit
- lower: parses a lowercase letter 
- upper: parses an uppercase letter
- letter: parses a letter
- alphanum: parses an alphanumeric char
- space: parses a single space or newline
- char x: parses a specific char x
- string s: parses a specific string s
- token p: parses a specific value p and ignores spaces
- symbol s: parses a specific symbol token s

#### 3.1.4 Variable parsing

In our language a variable is either an identifier or a location of an array. An identifier is an alphanumeric string starting with a letter; a location of an array is an identifier plus an arithmetic expression surrounded by square parenthesis.

```haskell
identifier ::  Parser Variable
identifier =
    do
        l <- letter
        ls <- many alphanum 
        if  isIdentifier (l:ls) then return (Identifier (l:ls)) else empty

variable :: Parser Variable 
variable =
    do
        id <- identifier
        do
            symbol "["
            a <- aexp
            symbol "]"
            return (ArrayLocation id a)
            <|> 
            return id 
```



#### 3.1.5 Arithmetic Expression Parsing

First we need to parse numbers (IMPterpreter works with doubles).

```haskell
nat :: Parser Double
nat =
    do
        ds <- some digit
        return (read ds)

positive :: Parser Double
positive =
    do
        int <- some digit
        dot <- char '.'
        decimal <- some digit
        return (read (int ++ [dot] ++ decimal))

number :: Parser Double
number = 
    positive
    <|>
    nat
    <|>
    (do
        char '-'
        num <- number
        return (- num)
    )

```

Then we can write the `aexp` parser. It builds a derivation tree in a left-associative way, so `4+5*4 = (4+5)*4`. Also the primary operations "+" and "-" have higher priority on "*" and "/". It uses the subparsers `aterm` and `afactor`.

```haskell
afactor :: Parser AExp
afactor = 
    (do Number <$> number) 
    <|>
    (do AVar <$> variable)
    <|>
    (do
        symbol "("
        a <- aexp
        symbol ")"
        return a
    )   
    <|> 
    (do
        symbol "-"
        Negation  <$> aexp
    )
```
```haskell
aterm :: Parser AExp
aterm = 
    do
        f <- afactor
        do
            op <- asecondaryop
            AExpOp f op <$> aterm
            <|>
            return f
```
```haskell
aexp :: Parser AExp
aexp = 
    do 
        t <- aterm
        do
            op <- aprimaryop
            AExpOp t op <$> aexp
            <|>
            return t
```

#### 3.1.6 Boolean expression parsing

The followed strategy is similar to the `aexp` parser:

```haskell
bfactor :: Parser BExp
bfactor =
    bool
    <|>
    (do
        a1 <- aexp
        c <- comparison
        Comparison a1 c <$> aexp
    )
    <|>
    (do BVar <$> variable)
    <|>
    (do
        symbol "("
        bx <- bexp
        symbol ")"
        return bx
    )
    <|>
    (do
        symbol "!"
        Not <$> bexp   
    )    
```
```haskell
bterm :: Parser BExp
bterm =
    do
        bf <- bfactor
        (do
            symbol "&&"
            BExpOp bf And <$> bfactor) 
            <|>
            return bf
```
```haskell
bexp :: Parser BExp
bexp =
    do
        bt <- bterm
        (do
            symbol "||"
            BExpOp bt Or <$> bexp) 
            <|>
            return bt
```

#### 3.1.7 Command parsing

The `program` parsers see a program as a single *Command* or as a *Sequence Command Program*. A comand is one of the basic construct of IMP.

```haskell
program :: Parser Program
program =
    do 
        c <- command
        do
            Sequence c <$> program
            <|>
            return (Single c) 

command :: Parser Command
command =
    skip <|>
    arraydeclaration <|>
    assignment <|>
    ifthenelse <|>
    while
```
`skip` parser:

```haskell
skip :: Parser Command
skip = 
    do
        symbol "skip"
        symbol ";"
        return Skip
```
IMPterpreter allow an extensional declaration of arrays, for example: `v = {1, 2, 3, 4, 5}`. The parser `arrayElements` parses the all the expressions contained in the extensional declaration. It can be noticed that this parser allow arrays with elements of different type.
<br>
Moreover we can declare an array giving the number of elements: `v = Array(n)`

```haskell
arrayElements :: Parser [Exp]
arrayElements =
    do
        b <- (do BExp <$> bexp)
        do
            symbol ","
            t <- arrayElements
            return (b:t)
            <|>
            return [b]
    <|>
    do
        a <- (do AExp <$> aexp)
        do
            symbol ","
            t <- arrayElements
            return (a:t)
            <|>
            return [a]

arraydeclaration :: Parser Command
arraydeclaration = 
    do
        v <- variable
        symbol "="
        do
            symbol "Array"
            symbol "("
            n <- aexp
            symbol ")"
            symbol ";"
            return (ArrayDeclaration (Intensional v n))
            <|> do
                symbol "{"
                exps <- arrayElements
                symbol "}"
                symbol ";"
                return (ArrayDeclaration (Extensional v exps))
```
The `assignment` parser consumes a variable followed by "=", and then we have 3 alternatives:

- aexp
- bexp
- variable

The "variable" can seem a bit redundant, as aexp and bexp contain it, but at the parsing level in an assignment like `x = y` the parser can not infer the type of y so we need this extra case.

```haskell
assignment :: Parser Command
assignment = 
    do
        v <- variable
        symbol "="
        do
            x <- variable
            symbol ";"
            return (Assignment v (Var x))
            <|> do
                e <- (do AExp <$> aexp)
                symbol ";"
                return (Assignment v e)
                <|> do
                    e <- (do BExp <$> bexp)
                    symbol ";"
                    return (Assignment v e)
```
The `ifthenelse` parser recognize the keyword **if**, subsequently it parses a boolean expression, and look for the **then** keyword. In the then-block as in the else-block is used the `program` parser. The IMP IfThenElse can omit the else-block and it ends with the **end** keyword.

```haskell
ifthenelse :: Parser Command
ifthenelse =
    do
        symbol "if"
        b <- bexp
        symbol "then"
        pthen <- program
        do
            symbol "else"
            pelse <- program
            symbol "end"
            return (IfThenElse b pthen pelse)
            <|> do
                symbol "end"
                return (IfThenElse b pthen (Single Skip))
```
The last parser is `while`, it look for the `while` keyword, then parse a boolean expression, subsequently it recognize the **do** keyword, parses a `program` and look for the **end** keyword.

```haskell
while :: Parser Command
while = 
    do
        symbol "while"
        b <- bexp
        symbol "do"
        p <- program
        symbol "end"
        return (While b p)
```

#### 3.1.8 Parse function

All the parsers are encapsulated by the function `parse` that returns the **Empty** program and the input string if the parsing fails, otherwise the parsed program and the not consumed part of the string.

```haskell
parse :: String -> (Program, String)
parse s = case p s of
  Nothing -> (Empty, s)
  Just (c, s) -> (c, s)
  where
    (P p) = program
```

**Examples:**

Successful parsing:

<img src="docs\imgs\parsingSuccess.png" alt="parsingSuccess" style="zoom:150%;" />

Failing parsing:

<img src="docs\imgs\parsingFail.png" alt="parsingFail" style="zoom:150%;" />

Partial success parsing:

<img src="docs\imgs\parsingPartialSuccess.png" alt="parsingPartialSuccess" style="zoom:150%;" />

### 3.2 Evaluator

The module Evaluator provides the functions to evaluate a program in the *internal tree representation* given an environment, and to produce a new environment.

#### 3.2.1 Environment

The **environment** is a list of variables:

```haskell
type Env = [Variable]
```

A **variable** is composed by a name and a value:

```haskell
data Variable = Variable {
    name :: String,
    value :: VType
} deriving Show
```

The three admitted **VTypes** are:

```haskell
data VType = TDouble Double | TBool Bool | TArray [VType]
```

The environment is managed by a *reading* and a *writing* function.

**getVarValue** returns value of the variable wrapped by **Maybe** given its name. If the name is not present returns **Nothing**.

```haskell
getVarValue :: Env -> String -> Maybe VType
getVarValue [] _ = Nothing
getVarValue (x:xs) qName = 
    if name x == qName
        then Just (value x)
    else getVarValue xs qName
```

**insertVar** insert a new variable in the environment or update an existing one. In the update the value can be arbitrary changed, modifying also its VType. 

```haskell
insertVar :: Env -> Variable -> Env
insertVar [] var = [var]
insertVar (x:xs) newVar =
    if name x == name newVar
        then newVar: xs
    else x : insertVar xs newVar
```

#### 3.2.2 Variable evaluation

**evalVar** given an identifier check if it exists in the environment and then returns its value, otherwise raise an error. If the variable is an array location it also evaluate is **index expression** and retrieve the array element.

```haskell
evalVar :: Env -> Tree.Variable -> VType
evalVar env (Identifier var) = 
    case getVarValue env var of
        Just (TDouble v) -> TDouble v
        Just (TBool v) -> TBool v
        Just (TArray arr) -> TArray arr
        Nothing -> error "VariableNotDefined"
evalVar env (ArrayLocation (Identifier m) a) =
    case getVarValue env m of
        Just (TArray v) -> execExpr env (AExp a) (getArrayElement v)
        Just _ -> error "TypeError"
        Nothing -> error "VariableNotDefined"
```
**safeEvalVar** check also that the variable is of a given VType.

```haskell
safeEvalVar :: Env -> Tree.Variable -> String -> VType
safeEvalVar env var varType = 
    case (evalVar env var, varType) of
        (TDouble d, "TDouble") -> TDouble d
        (TBool b, "TBool") -> TBool b
        (TArray a, "TArray") -> TArray a
        (_, _) -> error "TypeMismatch"     
```

#### 3.2.3 Arithmetic expression evaluation

**evalAExpr** evaluates an AExp and returns *Just* a double if the evaluations succeeds, otherwise *Nothing*

```haskell
evalAExpr :: Env -> AExp -> Maybe Double
evalAExpr _ (Number x) = Just x
evalAExpr env (Tree.AVar (Identifier v)) = Just (getDouble (safeEvalVar env (Identifier v) "TDouble"))
evalAExpr env (Tree.AVar (ArrayLocation v a)) = Just (getDouble (safeEvalVar env (ArrayLocation v a) "TDouble"))
evalAExpr env (AExpOp al op ar) = opADict op <$> evalAExpr env al <*> evalAExpr env ar
evalAExpr env (Negation a) = (-) <$> Just 0 <*> evalAExpr env a
```

#### 3.2.4 Boolean expression evaluation

**evalBExpr** evaluates a BExp and returns *Just* a bool if the evaluations succeeds, otherwise *Nothing*

```haskell
evalBExpr :: Env -> BExp -> Maybe Bool
evalBExpr _ (Boolean b) = Just b
evalBExpr env (Tree.BVar (Identifier v)) = Just (getBool (safeEvalVar env (Identifier v) "TBool"))
evalBExpr env (Tree.BVar (ArrayLocation v a)) = Just (getBool (safeEvalVar env (ArrayLocation v a) "TBool"))
evalBExpr env (BExpOp bl op br) = opBDict op <$> evalBExpr env bl <*> evalBExpr env br
evalBExpr env (Not b) = not <$> evalBExpr env b
evalBExpr env (Comparison al op ar) = opCDict op <$> evalAExpr env al <*> evalAExpr env ar
```

#### 3.2.5 Command evaluation

**execExpr** is a function used in the command evaluation that evaluate an arbitrary expression and if it returns *Just* a value apply a given *operation* on it. Otherwise raises an error. This functions is useful because it generalize a repeating pattern in the statement evalution.

```haskell
execExpr :: Env -> Exp -> (VType -> p) -> p
execExpr env e operation = 
    case e of
        AExp a ->
            case evalAExpr env a of
                Just x -> operation (TDouble x)
                Nothing -> error "ArithmeticError"
        BExp b ->
            case evalBExpr env b of
                Just x -> operation (TBool x)
                Nothing -> error "BooleanError"
        Var v ->
            operation (evalVar env v)
```

**execStatement** evaluates the commands of the program. Assignments are made out using **execExpr**, so evaluating expressions and then inserting the resulting value in the environment. The ifthenelse is executed evaluating the condition and then choosing the corresponding path. Lastly for the while loop we evaluate the conditions if is false we return the unchanged environment, otherwise we execute the commands in the do-block of the while, and then recursively call execStatement.

```haskell
execStatement :: Env -> Command -> Env
execStatement env Skip = env
execStatement env (Assignment (Identifier v) (AExp a)) =  execExpr env (AExp a) (insertVar env . Evaluator.Variable v)
execStatement env (Assignment (Identifier v) (BExp b)) =  execExpr env (BExp b) (insertVar env . Evaluator.Variable v)
execStatement env (Assignment (Identifier v) (Var w)) =  insertVar env  (Evaluator.Variable v (evalVar env w))
execStatement env (Assignment (ArrayLocation (Identifier v) av) (AExp a)) = execExpr env (AExp a) 
	(arrayElementAssignment env v av)
execStatement env (Assignment (ArrayLocation (Identifier v) av) (BExp b)) = execExpr env (BExp b) 
	(arrayElementAssignment env v av)
execStatement env (Assignment (ArrayLocation (Identifier v) av) (Var w)) = execExpr env (Var w) 
	(arrayElementAssignment env v av)
execStatement env (ArrayDeclaration (Intensional (Identifier v) a)) = execExpr env (AExp a) 
	(insertVar env . Evaluator.Variable v . TArray . declareArrayIntensional)
execStatement env (ArrayDeclaration (Extensional (Identifier v) exps)) = insertVar env (Evaluator.Variable v 
	(TArray (makeArray env exps)))
execStatement env (ArrayDeclaration (Extensional (ArrayLocation (Identifier v) av) exps)) = arrayElementAssignment env v av  		(TArray (makeArray env exps))

execStatement env (IfThenElse b pthen pelse) = execExpr env (BExp b) 
    (\bvalue -> if getBool bvalue then exec env pthen else exec env pelse)
execStatement env (While b p) = execExpr env (BExp b) 
    (\bvalue -> if getBool bvalue then execStatement (exec env p) (While b p) else env)
```

#### 3.2.5 Array management 

Arrays are stored in the environment as lists of VType, so they can have different VTypes for each element, even arrays.

The handling of elements is performed by the functions **getArrayElement** and **setArrayElement**. This functions can raise an `IndexOutOfRange` error if the index is higher or equal the array dimension.

```haskell
-- Get an element of an array given an index
getArrayElement :: [p] -> VType -> p
getArrayElement [] (TDouble index) = error "IndexOutOfRange"
getArrayElement (a:as) (TDouble index)
    | index == 0 = a
    | index > 0 = getArrayElement as (TDouble (index-1))

-- Set an element of an array given an index and a value
setArrayElement :: [t] -> VType -> t -> [t]
setArrayElement [] (TDouble index) v = error "IndexOutOfRange"
setArrayElement (a:as) (TDouble index) v
    | index == 0 = v:as
    | index > 0 = a:setArrayElement as (TDouble (index-1)) v
```

The intensional declaration is made out by the **makeArrayIntensional** function. The default values of arrays are `TDouble 0`.

```haskell
-- Construct an array given the number of elements
makeArrayIntensional :: VType -> [VType]
makeArrayIntensional (TDouble d)
    | d == 0 = []
    | d > 0  =  TDouble 0: makeArrayIntensional (TDouble (d-1))
```

**makeArrayExtensional** builds an array from a list of expressions, evaluating each one.

```haskell
-- Construct an arrat given the number of elements
makeArrayExtensional :: Env -> [Exp] -> [VType]
makeArrayExtensional env [exp] = execExpr env exp (:[])
makeArrayExtensional env (e:ex) = execExpr env e (\x -> x:makeArrayExtensional env ex)
```



## 4. Usage

Load the  **Main.hs** module from an haskell shell,  and then type `main` to launch IMPterpreter.



![image-20210103202322514](docs\imgs\main.png)

Instructions can be directly typed into the shell or can be loaded from file with the `:l` command.
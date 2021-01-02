# IMPterpreter

## 1. Introduction

An interpreter is a program that directly analyze and execute instructions written in a certain programming language. In this project was built a simple interpreter in Haskell for a simple imperative language called IMP.
The basic constructs of IMP are:
 - Skip: does nothing
 - Assignment: evaluate an rvalue and assign it to a variable
 - If then else: Performs the conditional selection between two paths
 - While: Loop a set of instructions according to a boolean condition

## 2. Grammar
W.t.r. to the original grammar of IMP, it has been modified to allow the representation of real numbers and arrays. The produced grammar is the following:
    
```EBNF
Program := <Command> | <Command> <Program>
Command ::= <Skip> | <ArrayDeclaration> | <Assignment> | <IfThenElse> | <While> 
Skip ::= "skip" <Semicolon> 
Assignment ::= <Variable> ":=" (AExp | BExp) <Semicolon>
IfThenElse ::= "if" <Space> <bexp> <Space> "then" <Space> (<Program> | <Program> "else" <Space> <Program>) "end" <Semicolon>
While ::= "while" <space> <BExp> <Space> "do" <Space> <Program> "end" <Semicolon> 
ArrayDeclaration ::= <Variable> "=" ( "Array" "(" <AExp> ")" | "{" (AExp | BExp) ["," (AExp | BExp)]* "}"  )

BExp ::= <BTerm> | <BTerm> "||" <BExp>
BTerm ::= <BFact> | <BTerm> "&&" <BFact>
BFact ::= <AExp> <ComparisonOp> <AExp> | <Bool> | <Variable> | "!" <BExp> | "(" <bexp> ")"

AExp ::= <ATerm> | <ATerm> "+" <AExp> | <ATerm> "-" <AExp>
ATerm = <AFactor> | <AFactor> "*" <ATerm> | <AFactor> "/" <ATerm>
AFactor = <PositiveNumber> | <Variable> | "(" <AExp> ")" | "-" <AExp>
PositiveNumber ::= <Digit> <PositiveNumber> | <PositiveNumber> "." <PositiveNumber> | <Digit>
Digit ::= [0-9]

Variable ::= [<Letter>]{<Letter> | <Letter>}
ComparisonOp ::= "<" | ">" | "=" | "<=" | ">=" | "!=" 
Letter ::= [a-z]
Semicolon ::= ";" | ";" <space> 
Spacee ::= " "  
``` 
 
## 3. Architecture and implementation

The interpreter is made out by two modules:
 - **The parser** : Given an input program processes it returning a tree representation of the program
 - **The evaluator** : Given the output of the parser and an initial enviroment, it executes the program producing a final enviroment 


### 3.1 Parser

The parser can be seen as a function that given a String, produces a represantation of the program in a tree structure, in this is case we see the tree structure as a generic type "a", moreover a part of the string or the entire can be not consumed sometimes, so it can return the unconsumed part and can return Nothing if the pasring completely fails; so the definition is: 

```Haskell
    newtype Parser a = P (String -> Maybe (a, String))
```
#### 3.1.1 Functor, Applicative, Monad, Alternative

These 4 instances are the core of the parser, because allow multiple parsers to operate in parallel and in sequence.

**Functor**

The Functor class provides the function *fmap*, in order to give the possibility to apply a function g to a wrapped value. In our implementation, if the result of out parsing is Nothing, it gives Nothing, otherwise it returns a result (v, out), we apply g to v and then wrap it in "Just". In any case we wrap the whole in the Parser type.

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

The Applicative class provides the function *pure* that simply wraps the given input, and the function *<\*>*  used to apply a wrapped function to a wrapped value. In our case we extract the wrapped function pg  

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

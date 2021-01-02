# IMPterpreter

## Introduction

An interpreter is a program that directly analyze and execute instructions written in a certain programming language. In this project was built a simple interpreter in Haskell for a simple imperative language called IMP.
The basic constructs of IMP are:
 - Skip: does nothing
 - Assignment: evaluate an rvalue and assign it to a variable
 - If then else: Performs the conditional selection between two paths
 - While: Loop a set of instructions according to a boolean condition

## Grammar
W.t.r. to the original grammar of IMP, it has been modified to allow the representation of real numbers and arrays. The produced grammar is the following:
    
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
    
## Architecture

The interpreter is made out by two modules:
 - **The parser** : Given an input program processes it returning a tree representation of the program
 - **The evaluator** : Given the output of the parser and an initial enviroment, it executes the program producing a final enviroment 


### Parser

The parser can be seen as a function that given a String, produces a represantation of the program in a tree structure, in this is case we see the tree structure as a generic type "a", moreover a part of the string can be not consumed sometimes, so it can return the unconsumed part; so the definition is: 

    newtype Parser a = P (String -> Maybe (a, String))
    


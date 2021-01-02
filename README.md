# IMPterpreter


## Grammar
 
    
    Program := <Command> | <Command> <Program>
    Command ::= <Skip> | <ArrayDeclaration> | <Assignment> | <IfThenElse> | <While> 
    Skip ::= "skip" <Semicolon> 
    Assignment ::= <Variable> ":=" (AExp | BExp) <Semicolon>
    IfThenElse ::= "if" <Space> <bexp> <Space> "then" <Space> (<Program> | <Program> "else" <Space> <Program>) "end" <Semicolon>
    While ::= "while" <space> <BExp> <Space> "do" <Space> <Program> "end" <Semicolon> 
    
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

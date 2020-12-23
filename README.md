# IMPterpreter


## Grammar
 
    
    program := <command> | <command> <program>
    command ::= <skip> | <assignment> | <ifthenelse> | <while> 
    skip ::= "skip" <semicolon> 
    assignment ::= <variable> ":=" (<aexp> | <bexp>) <semicolon>
    ifthenelse ::= "if" <space> <bexp> <space> "then" <space> (<program> | <program> "else" <space> <program>) "end" <semicolon>
    while ::= "while" <space> <bexp> <space> "do" <space> <program> "end" <semicolon> 
    
    bexp ::= <bterm> | <bterm> <bop> <bexp> | "(" <bexp> ")" | "!" <bexp>
    bterm::= <aexp> | <aexp> <comparisonop> <aexp> | <variable> 
    bop ::= "&&" | "||" 
    
    aexp ::= <aterm> | <aterm> <aop> <aexp> | "(" <aexp> ")" | "-" <aexp>
    aterm = <positivenumber> | <variable>
    positivenumber ::= <digit> <positivenumber> | <positivenumber> "." <positivenumber> | <digit>
    aop ::= "+" | "-" | "*" | "/" 
    digit ::= [0-9]
    
    variable ::= [<letter>]{<letter> | <digit>}
    comparisonOp ::= "<" | ">" | "=" | "<=" | ">=" | "!=" 
    letter ::= [a-z]
    semicolon ::= ";" | ";" <space> 
    space ::= " "  

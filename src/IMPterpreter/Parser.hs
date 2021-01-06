module IMPterpreter.Parser where
import IMPterpreter.Tree (AExp (..), BExp (..), Command (..), ComparisonOp (..), AOp (..), BOp (..), Exp (..), Variable (..), Program (..))
import IMPterpreter.Functions
    ( isSpace, isAlphaNum, isAlpha, isUpper, isLower, isDigit, isIdentifier)


newtype Parser a = P (String -> Maybe (a, String))

parse :: String -> (Program, String)
parse s = case p s of
  Nothing -> (Empty, s)
  Just (c, s) -> (c, s)
  where
    (P p) = program

useparse :: Parser a -> String -> Maybe (a, String)
useparse (P p)  = p 

isParseFailed :: (Program, String) -> Bool
isParseFailed (_, "") = False
isParseFailed (_, _) = True

instance Functor Parser where
    fmap g (P p) =
        P
        ( \input -> case p input of
            Nothing -> Nothing
            Just (v, out) -> Just (g v, out)
        )

instance Applicative Parser where
    pure v = P (\input -> Just (v, input))
    (P pg) <*> px =
        P
        ( \input -> case pg input of
            Nothing -> Nothing
            Just (g, out) -> case fmap g px of
                (P p) -> p out
        )

instance Monad Parser where
    (P p) >>= f =
        P
        ( \input -> case p input of
            Nothing -> Nothing
            Just (v, out) -> case f v of
                (P p) -> p out
        )

instance Alternative Parser where
    empty = P (const Nothing)
    (P p) <|> (P q) =
        P
        ( \input -> case p input of
            Nothing -> q input
            Just (v, out) -> Just (v, out)
        )

instance Alternative Maybe where
    empty = Nothing
    Nothing <|> my = my
    (Just x) <|> _ = Just x

class Monad f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
    many :: f a -> f [a]
    some :: f a -> f [a]
    many x = some x <|> pure []
    some x = ((:) <$> x) <*> many x


------ Atomic element parsing ------

-- Parses a single char
item :: Parser Char
item =
  P
    ( \input -> case input of
        [] -> Nothing
        (x : xs) -> Just (x, xs)
    )

-- Tells if a predicate p is satisfied
sat :: (Char -> Bool) -> Parser Char
sat p = 
    do
        x <- item;
        if p x then return x else empty;

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

space :: Parser ()
space = 
    do 
        many (sat isSpace);
        return ();
 
char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = 
    do 
        char x;
        string xs;
        return (x:xs);

symbol :: String -> Parser String
symbol xs = token (string xs)

token :: Parser a -> Parser a
token p = 
    do 
        space;
        v <- p;
        space;
        return v;


------ Arithmetic expression parsing ------

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
            arrayindexes id
            <|> 
            return id 

arrayindexes :: Variable -> Parser Variable
arrayindexes id =
    do
        symbol "["
        a <- aexp
        symbol "]"
        do
            v <- arrayindexes id
            return (ArrayLocation v a)
            <|>
            return (ArrayLocation id a)

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
        symbol "("
        a <- aexp
        symbol ")"
        return (Negation a)
    )

aterm :: Parser AExp
aterm = 
    do 
        lf <- afactor
        rest lf 
    where rest lf = (do op <- asecondaryop; rf <- afactor; rest (AExpOp lf op rf)) <|> return lf

aprimaryop :: Parser AOp
aprimaryop =
    (do op <- symbol "+"; return Add) <|>
    (do op <- symbol "-"; return Sub)

asecondaryop :: Parser AOp
asecondaryop =
    (do op <- symbol "*"; return Mul) <|>
    (do op <- symbol "/"; return Div)

-- aexp :: Parser AExp
-- aexp = 
--     do 
--         t <- aterm
--         do
--             op <- aprimaryop
--             AExpOp t op <$> aexp
--             <|>
--             return t

aexp :: Parser AExp
aexp = 
    do 
        lt <- aterm
        rest lt 
    where rest lt = (do op <- aprimaryop; rt <- aterm; rest (AExpOp lt op rt)) <|> return lt



------ Boolean expression parsing ------

comparison :: Parser ComparisonOp
comparison =
    do
        symbol "<="
        return Le
    <|> 
    do
        symbol ">="
        return Ge
    <|> 
    do
        symbol "<"
        return Lt
    <|>
    do
        symbol ">"
        return Gt
    <|> 
    do
        symbol "=="
        return Eq
    <|> 
    do
        symbol "!="
        return Neq

bool :: Parser BExp
bool = 
    (do
        s <- symbol "True"
        return (Boolean True)
        )
    <|>
    (do
        s <- symbol "False"
        return (Boolean False)
        )

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


bterm :: Parser BExp
bterm =
    do 
        lf <- bfactor
        rest lf 
    where rest lf = (do symbol "&&"; rf <- bfactor; rest (BExpOp lf Or rf)) <|> return lf

bexp :: Parser BExp
bexp =
    do 
        lt <- bterm
        rest lt 
    where rest lt = (do symbol "||"; rt <- bterm; rest (BExpOp lt And rt)) <|> return lt


------ Command expression parsing ------

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
    assignment <|>
    ifthenelse <|>
    while
    
skip :: Parser Command
skip = 
    do
        symbol "skip"
        symbol ";"
        return Skip

arrayelements :: Parser [Exp]
arrayelements =
    do
        v <- (do Var <$> variable)
        do
            symbol ","
            t <- arrayelements
            return (v:t)
            <|>
            return [v]
    <|>
    do
        b <- (do BExp <$> bexp)
        do
            symbol ","
            t <- arrayelements
            return (b:t)
            <|>
            return [b]
    <|>
    do
        a <- (do AExp <$> aexp)
        do
            symbol ","
            t <- arrayelements
            return (a:t)
            <|>
            return [a]
    <|>
    do
        a <- arraydeclaration
        do
            symbol ","
            t <- arrayelements
            return (a:t)
            <|>
            return [a]

arraydeclaration :: Parser Exp
arraydeclaration = 
    do
        symbol "array"
        symbol "("
        n <- aexp
        symbol ")"
        return (ArrIntensional n)
    <|> 
    do
        symbol "["
        exps <- arrayelements
        symbol "]"
        return (ArrExtensional exps)

assignment :: Parser Command
assignment = 
    do
        v <- variable
        symbol "="
        Assignment v <$> rightValue


rightValue :: Parser Exp
rightValue =
    do
        x <- variable
        symbol ";"
        return (Var x)
    <|> 
    do
        e <- (do AExp <$> aexp)
        symbol ";"
        return e
    <|> 
        do
        e <- (do BExp <$> bexp)
        symbol ";"
        return e
    <|> 
    do
        e <- arraydeclaration
        symbol ";"
        return e

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
    
while :: Parser Command
while = 
    do
        symbol "while"
        b <- bexp
        symbol "do"
        p <- program
        symbol "end"
        return (While b p)

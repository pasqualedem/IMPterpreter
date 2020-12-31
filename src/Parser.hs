module Parser where
import Grammar (AExp (..), BExp (..), Command (..), ComparisonOp (..), AOp (..), BOp (..), Exp (..), Variable (..), Program (..), ArrDecl (..))
import Functions
    ( isSpace, isAlphaNum, isAlpha, isUpper, isLower, isDigit )


newtype Parser a = P (String -> Maybe (a, String))

parse :: String -> (Program, String)
parse s = case p s of
  Nothing -> (Empty, "")
  Just (c, s) -> (c, s)
  where
    (P p) = program

parseFailed :: (Program, [Char]) -> Bool
parseFailed (Empty, "") = True
parseFailed (_, "") = False
parseFailed (_, _) = False

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
  --(>>=) :: m a -> (a -> m b) -> m b
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
    ( \inp -> case inp of
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
 do {
  space;
  v <- p;
  space;
  return v;
 }

------ Arithmetic expression parsing ------

nat :: Parser Double
nat =
    do
        ds <- some digit
        return (read ds)

double :: Parser Double
double =
    do
        nat1 <- some digit
        dot <- char '.'
        nat2 <- some digit
        return (read (nat1 ++ [dot] ++ nat2))

number :: Parser Double
number = 
    double
    <|>
    nat
    <|>
    (do
        char '-'
        num <- number
        return (- num)
    )

variable :: Parser Variable 
variable =
    do
        ltr <- letter
        ls <- many alphanum
        do
            symbol "["
            a <- aexp
            symbol "]"
            return (MemLocation (ltr : ls) a)
            <|> do 
            return (Identifier (ltr : ls))

afactor :: Parser AExp
afactor = 
    (do Number <$> number) 
    <|>
    (do AVariable <$> variable)
    <|>
    (do 
        symbol "-"
        Negation . AVariable <$> variable
    )
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
        f <- afactor
        do
            op <- asecondaryop
            AExpOp f op <$> aterm
            <|>
            return f

aprimaryop :: Parser AOp
aprimaryop =
    (do op <- symbol "+"; return Add) <|>
    (do op <- symbol "-"; return Sub)

asecondaryop :: Parser AOp
asecondaryop =
    (do op <- symbol "*"; return Mul) <|>
    (do op <- symbol "/"; return Div)

aexp :: Parser AExp
aexp = 
    do 
        t <- aterm
        do
            op <- aprimaryop
            AExpOp t op <$> aexp
            <|>
            return t



------ Boolean expression parsing ------

comparison :: Parser ComparisonOp
comparison =
    do
        symbol "<"
        return Lt
    <|>
    do
        symbol "<="
        return Le
    <|> 
    do
        symbol ">"
        return Gt
    <|> 
    do
        symbol ">="
        return Ge
    <|> 
    do
        symbol "=="
        return Eq
    <|> 
    do
        symbol "!="
        return Neq

bcomparison :: Parser ComparisonOp
bcomparison =
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
    (do BVariable <$> variable)
    <|>    
    (do 
        symbol "!"
        Not . BVariable <$> variable
    )
    <|>
    (do
        nt <- symbol "!"
        symbol "("
        bx <- bexp
        symbol ")"
        return (Not bx)   
    )    
    <|>
    (do
        symbol "("
        bx <- bexp
        symbol ")"
        return bx
    )

bterm :: Parser BExp
bterm =
    do
        bf <- bfactor
        (do
            symbol "&&"
            BExpOp bf And <$> bfactor) 
            <|>
            return bf

bexp :: Parser BExp
bexp =
    do
        bt <- bterm
        (do
            symbol "||"
            BExpOp bt Or <$> bexp) 
            <|>
            return bt


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
    arraydeclaration <|>
    assignment <|>
    ifthenelse <|>
    while
    
skip :: Parser Command
skip = 
    do
        symbol "skip"
        symbol ";"
        return Skip

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
                return (ArrayDeclaration (Extensional exps))

assignment :: Parser Command
assignment = 
    do
        v <- variable
        symbol "="
        do
            e <- (do BExp <$> bexp)
            symbol ";"
            return (Assignment v e)
            <|> do
                e <- (do AExp <$> aexp)
                symbol ";"
                return (Assignment v e)
    
ifthenelse :: Parser Command
ifthenelse =
    do
        symbol "if"
        symbol "("
        b <- bexp
        symbol ")"
        symbol "then"
        symbol "{"
        pthen <- program
        symbol "}"
        symbol "else"
        symbol "{"
        pelse <- program
        symbol "}"
        symbol ";"
        return (IfThenElse b pthen pelse)
    <|> 
    do
        symbol "if"
        symbol "("
        b <- bexp
        symbol ")"
        symbol "then"
        symbol "{"
        pthen <- program
        symbol "}"
        symbol ";"
        return (IfThenElse b pthen (Single Skip))
    

while :: Parser Command
while = 
    do
        symbol "while"
        symbol "("
        b <- bexp
        symbol ")"
        symbol "do"
        symbol "{"
        p <- program
        symbol "}"
        symbol ";"
        return (While b p)

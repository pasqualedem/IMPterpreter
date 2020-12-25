module Parser where
import Grammar (AExp (..), BExp (..), Command (..), ComparisonOp (..), AOp (..), ATerm (..), Minus (..), BTerm (..), BOp (..), Not (..), Exp (..), Variable (..), Program (..))
import Functions
    ( isSpace, isAlphaNum, isAlpha, isUpper, isLower, isDigit )


newtype Parser a = P (String -> Maybe (a, String))

parse :: String -> (Program, String)
parse s = case p s of
  Nothing -> (Empty, "")
  Just (c, s) -> (c, s)
  where
    (P p) = program

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

class Applicative f => Alternative f where
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
    nat
    <|>
    double
    <|>
    (do
        num <- number
        char '-'
        return (- num)
    )

variable :: Parser Variable 
variable =
    do
        ltr <- letter
        a <- some alphanum
        return (AlphaVar (ltr : a))
        
aterm :: Parser ATerm
aterm = 
    (do Number <$> number) <|>
    (do AVariable <$> variable)

aop :: Parser AOp
aop =
    (do op <- symbol "+"; return Add) <|>
    (do op <- symbol "-"; return Sub) <|>
    (do op <- symbol "*"; return Mul) <|>
    (do op <- symbol "/"; return Div)

aexp :: Parser AExp
aexp = 
    (do 
        t <- aterm
        op <- aop
        a <- aexp
        ATermOp t op <$> aexp
    )
    <|>
    (do 
        a <- aexp
        op <- aop
        AExpOp a op <$> aexp
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
        a <- aexp
        Negation Minus <$> aexp
    )
    <|>
    do ATerm <$> aterm


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

bool :: Parser BTerm
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

bop :: Parser BOp
bop = 
    (do
        s <- symbol "||"
        return Or
        )
    <|>
    (do
        s <- symbol "&&"
        return And
        )

bterm :: Parser BTerm
bterm =
    (do
        a1 <- aexp
        c <- comparison
        Comparison a1 c <$> aexp
    )
    <|>
    (do ABExp <$> aexp)
    <|>
    (do BVariable <$> variable)
    <|>
    bool

bexp :: Parser BExp
bexp =
    (do
        symbol "("
        bx <- bexp
        symbol ")"
        return bx
    )
    <|>
    (do
        bt <- bterm
        op <- bop
        BTermOp bt op <$> bexp
    )
    <|>
    (do
        bxl <- bexp
        op <- bop
        BExpOp bxl op <$> bexp
    )
    <|>
    (do
        nt <- symbol "!"
        Inversion Not <$> bexp   
    )
    <|>
    do BTerm <$> bterm


------ Command expression parsing ------

program :: Parser Program
program =
    do Single <$> command
    <|>
    (do 
        c <- command
        Program c <$> program
    )

command :: Parser Command
command =
    skip <|>
    assignment <|>
    ifthenelse <|>
    while


expr :: Parser Exp
expr = 
    (do BExp <$> bexp) <|>
    (do AExp <$> aexp)
    
skip :: Parser Command
skip = 
    do
        symbol "skip"
        symbol ";"
        return Skip

assignment :: Parser Command
assignment = 
    do
        v <- variable
        symbol "="
        e <- expr
        symbol ";"
        Assignment v <$> expr

ifthenelse :: Parser Command
ifthenelse =
    do
        symbol "if"
        b <- bexp
        symbol "then"
        pthen <- program
        symbol "else"
        pelse <- program
        symbol ";"
        return (IfThenElse b pthen pelse)
    <|> 
    do
        symbol "if"
        b <- bexp
        symbol "then"
        pthen <- program
        symbol ";"
        return (IfThenElse b pthen (Single Skip))
    

while :: Parser Command
while = 
    do
        symbol "while"
        b <- bexp
        symbol "do"
        p <- program
        symbol ";"
        return (While b p)



    

    

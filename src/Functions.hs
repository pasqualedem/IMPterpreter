module Functions where

isLower :: Char -> Bool
isLower c = c `elem` ['a'..'z']

isUpper :: Char -> Bool
isUpper c = c `elem` ['A'..'Z']

isSpace :: Char -> Bool
isSpace c = c `elem` ['\n', '\t', '\r', ' ']

isAlpha :: Char -> Bool
isAlpha c = isUpper c || isLower c


isDigit :: Char -> Bool
isDigit c = c `elem` ['0' .. '9']

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c

isKeyword :: [Char] -> Bool
isKeyword s =  s `elem` ["True", "False", "if", "then", "else", "do", "end", "Array"]

isIdentifier :: [Char] -> Bool
isIdentifier [c] = isAlpha c
isIdentifier s = not(isKeyword s)

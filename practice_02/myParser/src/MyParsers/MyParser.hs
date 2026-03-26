module MyParsers.MyParser (
    Parser,
    runParser,
    satisfy,
    char,
    digit,
    digits,
    plusOrMult
) where

import MyTypes.MyMaybe
import Data.Char (isDigit)

newtype Parser tok a = Parser {runParser :: [tok] -> MyMaybe ([tok], a)}

instance Functor (Parser tok) where
    fmap g (Parser u) = Parser f where
        f xs = case u xs of
            MyNothing -> MyNothing
            MyJust (toks', x) -> MyJust (toks', g x)

instance Applicative (Parser tok) where
    pure x = Parser $ \xs -> MyJust (xs, x)
    
    Parser u <*> Parser v = Parser f where
        f xs = case u xs of
            MyNothing -> MyNothing
            MyJust (toks, x) -> case v toks of
                MyNothing -> MyNothing
                MyJust (toks', x') -> MyJust (toks', x x')

instance Alternative (Parser tok) where
    empty = Parser $ \_ -> MyNothing
    
    Parser u <|> Parser v = Parser f where
        f xs = case u xs of
            MyNothing -> v xs
            MyJust x -> MyJust x

satisfy :: (tok -> Bool) -> Parser tok tok
satisfy p = Parser f where
    f (x:xs) | p x = MyJust (xs, x)
    f _ = MyNothing

char :: Char -> Parser Char Char
char c = satisfy (== c)

digit :: Parser Char Int
digit = (\c -> digitToInt c) <$> satisfy isDigit
    where
        digitToInt '0' = 0
        digitToInt '1' = 1
        digitToInt '2' = 2
        digitToInt '3' = 3
        digitToInt '4' = 4
        digitToInt '5' = 5
        digitToInt '6' = 6
        digitToInt '7' = 7
        digitToInt '8' = 8
        digitToInt '9' = 9
        digitToInt _ = error "Not a digit"

digits :: Parser Char Int
digits = foldl (\acc d -> acc * 10 + d) 0 <$> some digit
    where
        some p = (:) <$> p <*> many p
        many p = some p <|> pure []

mult :: Parser Char Int
mult = (*) <$> digits <* char '*' <*> digits

plus :: Parser Char Int
plus = (+) <$> digits <* char '+' <*> digits

plusOrMult :: Parser Char Int
plusOrMult = mult <|> plus
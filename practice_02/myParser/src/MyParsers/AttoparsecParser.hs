module MyParsers.AttoparsecParser (
    plusOrMultAttoparsec,
    runParserAttoparsec
) where

import Data.Attoparsec.Text
import qualified Data.Text as T
import Data.Char (isDigit)
import MyTypes.MyMaybe

digitAttoparsec :: Parser Int
digitAttoparsec = do
    c <- satisfy isDigit
    return (digitToInt c)
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

digitsAttoparsec :: Parser Int
digitsAttoparsec = do
    ds <- many1 digitAttoparsec
    return (foldl (\acc d -> acc * 10 + d) 0 ds)

multAttoparsec :: Parser Int
multAttoparsec = do
    x <- digitsAttoparsec
    char '*'
    y <- digitsAttoparsec
    return (x * y)

plusAttoparsec :: Parser Int
plusAttoparsec = do
    x <- digitsAttoparsec
    char '+'
    y <- digitsAttoparsec
    return (x + y)

plusOrMultAttoparsec :: Parser Int
plusOrMultAttoparsec = multAttoparsec <|> plusAttoparsec

runParserAttoparsec :: Parser Int -> String -> MyMaybe Int
runParserAttoparsec p input = 
    case parseOnly p (T.pack input) of
        Left _ -> MyNothing
        Right result -> MyJust result
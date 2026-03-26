module MyParsers.ParsecParser (
    plusOrMultParsec,
    runParserParsec
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char (isDigit)
import MyTypes.MyMaybe

digitParsec :: Parser Int
digitParsec = do
    c <- oneOf "0123456789"
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

digitsParsec :: Parser Int
digitsParsec = do
    ds <- many1 digitParsec
    return (foldl (\acc d -> acc * 10 + d) 0 ds)

multParsec :: Parser Int
multParsec = do
    x <- digitsParsec
    char '*'
    y <- digitsParsec
    return (x * y)

plusParsec :: Parser Int
plusParsec = do
    x <- digitsParsec
    char '+'
    y <- digitsParsec
    return (x + y)

plusOrMultParsec :: Parser Int
plusOrMultParsec = multParsec <|> plusParsec

runParserParsec :: Parser Int -> String -> MyMaybe Int
runParserParsec p input = 
    case parse p "" input of
        Left _ -> MyNothing
        Right result -> MyJust result
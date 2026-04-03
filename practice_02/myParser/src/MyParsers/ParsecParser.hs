module MyParsers.ParsecParser (
    digitParsec,
    digitsParsec,
    multParsec,
    plusParsec,
    plusOrMultParsec,
    runParserParsec
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char (isDigit, digitToInt)
import MyTypes.MyMaybe
import qualified Control.Applicative as A

digitParsec :: Parser Int
digitParsec = digitToInt <$> satisfy isDigit

digitsParsec :: Parser Int
digitsParsec = fmap (foldl (\x y -> x * 10 + y) 0) (many1 digitParsec)

multParsec :: Parser Int
multParsec = (*) <$> digitsParsec <* char '*' <*> digitsParsec

plusParsec :: Parser Int
plusParsec = (+) <$> digitsParsec <* char '+' <*> digitsParsec

plusOrMultParsec :: Parser Int
plusOrMultParsec = multParsec A.<|> plusParsec

runParserParsec :: Parser Int -> String -> MyMaybe Int
runParserParsec p input = 
    case parse p "" input of
        Left _ -> MyNothing
        Right result -> MyJust result
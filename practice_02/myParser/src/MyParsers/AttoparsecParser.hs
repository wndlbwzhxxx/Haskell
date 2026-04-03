module MyParsers.AttoparsecParser (
    digitAttoparsec,
    digitsAttoparsec,
    multAttoparsec,
    plusAttoparsec,
    plusOrMultAttoparsec,
    runParserAttoparsec
) where

import Data.Attoparsec.Text
import qualified Data.Text as T
import Data.Char (isDigit, digitToInt)
import MyTypes.MyMaybe
import qualified Control.Applicative as A

digitAttoparsec :: Parser Int
digitAttoparsec = digitToInt <$> satisfy isDigit

digitsAttoparsec :: Parser Int
digitsAttoparsec = foldl (\acc d -> acc * 10 + d) 0 <$> many1 digitAttoparsec

multAttoparsec :: Parser Int
multAttoparsec = (*) <$> digitsAttoparsec <* char '*' <*> digitsAttoparsec

plusAttoparsec :: Parser Int
plusAttoparsec = (+) <$> digitsAttoparsec <* char '+' <*> digitsAttoparsec

plusOrMultAttoparsec :: Parser Int
plusOrMultAttoparsec = multAttoparsec A.<|> plusAttoparsec

runParserAttoparsec :: Parser Int -> String -> MyMaybe Int
runParserAttoparsec p input = 
    case parseOnly p (T.pack input) of
        Left _ -> MyNothing
        Right result -> MyJust result
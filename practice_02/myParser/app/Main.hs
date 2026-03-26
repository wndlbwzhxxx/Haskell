module Main where

import MyTypes.MyMaybe
import MyParsers.MyParser (plusOrMult, runParser)
import MyParsers.ParsecParser (plusOrMultParsec, runParserParsec)
import MyParsers.AttoparsecParser (plusOrMultAttoparsec, runParserAttoparsec)

main :: IO ()
main = do
    putStrLn "Модули MyTree, MyMaybe и MyEither успешно импортированы"

    putStrLn "MyParser:"
    putStrLn $ show (runParser plusOrMult "12*345dsf")
    putStrLn $ show (runParser plusOrMult "12+345dsf")
    putStrLn "Parsec:"
    putStrLn $ show (runParserParsec plusOrMultParsec "12*345dsf")
    putStrLn $ show (runParserParsec plusOrMultParsec "12+345dsf")
    putStrLn "Attoparsec:"
    putStrLn $ show (runParserAttoparsec plusOrMultAttoparsec "12*345dsf")
    putStrLn $ show (runParserAttoparsec plusOrMultAttoparsec "12+345dsf")
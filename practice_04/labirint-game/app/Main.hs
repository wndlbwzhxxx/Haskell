module Main where

import MyStateT
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
    let comp :: MyStateT Int IO Int
        comp = do
            x <- get
            put (x + 1)
            return x
    (res, st) <- runState comp 5
    liftIO $ print res
    liftIO $ print st
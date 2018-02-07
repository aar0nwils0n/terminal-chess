module Main where

import Lib
import Board
import Data.Map.Lazy

printBoard pieces = putStrLn $ Prelude.foldl (\x y -> x ++ "\n" ++ y)  "" (createBoard board pieces (keys pieces))

playGame pieces = do
    command <- getLine
    do
        printBoard pieces
        do
            playGame pieces
        

main :: IO ()
main = do
    printBoard initPieces
    do
        playGame initPieces


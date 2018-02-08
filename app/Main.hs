module Main where

import Board
import Data.Map.Lazy
import Data.Char (digitToInt)

printBoard pieces = putStrLn $ Prelude.foldl (\x y -> x ++ "\n" ++ y)  "" (createBoard board pieces (keys pieces))

playGame pieces = do
    command <- getLine
    do
        let newPieces = movePiece command pieces
        printBoard newPieces
        do
            playGame newPieces

movePiece :: String -> Map (Int, Int) Char -> Map (Int, Int) Char
movePiece c ps = 
    if (length c) /= 5 then ps
    else let 
        x1 = digitToInt (c !! 0)
        y1 = digitToInt (c !! 1)
        x2 = digitToInt (c !! 3)
        y2 = digitToInt (c !! 4)
        p = ps ! (x1, y1)
        in insert (x2, y2) p . delete (x2, y2) $ delete (x1, y1) ps
        

main :: IO ()
main = do
    printBoard initPieces
    do
        playGame initPieces


module GamePlay where

import Board
import Data.Map.Lazy
import Data.Char (digitToInt)
import Data.Either (isRight, fromLeft, fromRight)
import PieceBehavior (validateMove)

printBoard pieces = Prelude.foldl (\x y -> x ++ "\n" ++ y)  "" (createBoard board pieces (keys pieces))

playGame pieces = do
    putStrLn "Enter move"
    do
        command <- getLine
        do
            let parsed = parseCommand command pieces
                newPieces = movePiece parsed pieces
            putStrLn (if isRight parsed then fromRight "" parsed
            else printBoard newPieces)
            do
                playGame newPieces

parseCommand :: String -> Map (Int, Int) Char -> Either ((Int, Int), (Int, Int)) String
parseCommand c ps = if length c /= 5 then Right "Invalid command"
            else 
                let moves = ((digitToInt $ (!!) c 0, digitToInt $ (!!) c 1), (digitToInt $ (!!) c 3, digitToInt $ (!!) c 4))
                in
                    if validateMove (ps ! fst moves) (fst moves) (snd moves) ps == False 
                        then Right "Invalid move"
                    else Left moves

movePiece :: Either ((Int, Int), (Int, Int)) String -> Map (Int, Int) Char -> Map (Int, Int) Char
movePiece c ps = 
    let e = ((0, 0), (0, 0))
    in
    if isRight c then ps
    else let 
        x1 = fst . fst $ fromLeft e c
        y1 = snd . fst $ fromLeft e c
        x2 = fst . snd $ fromLeft e c
        y2 = snd . snd $ fromLeft e c
        p = ps ! (x1, y1)
        in insert (x2, y2) p . delete (x2, y2) $ delete (x1, y1) ps
        
startGame = do
    putStrLn $ printBoard initPieces
    do
        playGame initPieces
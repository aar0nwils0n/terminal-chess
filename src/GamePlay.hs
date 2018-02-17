module GamePlay where

import Board
import Data.Map.Lazy
import Data.Char (digitToInt)
import Data.Either (isRight, fromLeft, fromRight)
import PieceBehavior (validateMove, isWhite, isBlack)

printBoard pieces = Prelude.foldl (\x y -> x ++ "\n" ++ y)  "" (createBoard board pieces (keys pieces))

playGame pieces color = do
    putStrLn $ "Move " ++ color ++ " piece"
    do
        command <- getLine
        do
            let parsed = parseCommand command pieces color
                newPieces = movePiece parsed pieces
            putStrLn (if isRight parsed then fromRight "" parsed
            else printBoard newPieces)
            do
                playGame newPieces (
                        if isRight parsed then color
                        else if color == "white" then "black" else "white"
                    )

parseCommand :: String -> Map (Int, Int) Char -> String -> Either ((Int, Int), (Int, Int)) String
parseCommand c ps color = if length c /= 5 then Right "Invalid command"
            else 
                let og = (digitToInt $ (!!) c 0, digitToInt $ (!!) c 1)
                    nx = (digitToInt $ (!!) c 3, digitToInt $ (!!) c 4)
                    moves = (og, nx)
                in
                    if ps !? og == Nothing then Right "Piece does not exist"
                    else if verifyMoveColor og ps color == False then Right "Wrong color"
                    else if validateMove (ps ! og) og nx ps == False then Right "Invalid move"
                    else Left moves

verifyMoveColor :: (Int, Int) -> Map (Int, Int) Char -> String -> Bool
verifyMoveColor k ps color
    | color == "white" = isWhite $ ps ! k
    | color == "black" = isBlack $ ps ! k
    | otherwise = False

                    
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
        playGame initPieces "white"
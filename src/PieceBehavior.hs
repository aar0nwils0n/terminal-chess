module PieceBehavior where

import Data.Maybe (fromJust)
import Data.Map.Lazy

validWhitePawn :: (Int, Int) -> (Int, Int) -> Map (Int, Int) Char -> Bool
validWhitePawn og nx ps = let 
    x = fst nx - fst og
    y = snd nx - snd og
    in inBounds nx && ((
        x == 0 && (y == 1 || (y == 2 && snd og == 1))
    ) || (y == 1 && (
        x == 1 || x == -1
    ) && oppositeColorPieceExists ps nx isBlack))

oppositeColorPieceExists :: Map (Int, Int) Char -> (Int, Int) -> (Char -> Bool) -> Bool
oppositeColorPieceExists ps nx f =
    ps !? nx /= Nothing && (f . fromJust $ ps !? nx)

validBlackPawn og nx ps = let 
    x = fst nx - fst og
    y = snd nx - snd og
    in inBounds nx && ((
        x == 0 && (y == -1 || (y == -2 && snd og == 6))
    ) || (y == -1 && (
        x == 1 || x == -1
    ) && oppositeColorPieceExists ps nx isWhite))

validKnight og nx = let
    x = abs $ (-) (fst og) (fst nx)
    y = abs $ (-) (snd og) (snd nx)
    in inBounds nx
    && ((x == 2 && y == 1) || (y == 2 && x == 1))

validBishop og nx = let
    x = abs $ (-) (fst og) (fst nx)
    y = abs $ (-) (snd og) (snd nx)
    in inBounds nx
    && (x == y)

validRook og nx = let
    x = abs $ (-) (fst og) (fst nx)
    y = abs $ (-) (snd og) (snd nx)
    in inBounds nx
    && (
        (x > 0 && y == 0) || (y > 0 && x == 0)
    )
    
inBounds nx = let
    x = fst nx
    y = snd nx
    in (x <= 7) && (x >= 0)
    && (y <= 7) && (y >= 0)

validKing og nx = let
    x = abs $ (-) (fst og) (fst nx)
    y = abs $ (-) (snd og) (snd nx)
    in x < 2 && y < 2 && (x /= 0 || y /= 0)

validateMove :: Char -> (Int, Int) -> (Int, Int) -> Map (Int, Int) Char -> Bool
validateMove c og nx ps = 
    validateTakeOver c nx ps
    && validatePieceBehavior c og nx ps
    && (c == '♘' || c == '♞' || False == piecesBetween og (fst nx) (snd nx) ps)


validateTakeOver :: Char -> (Int, Int) -> Map (Int, Int) Char -> Bool
validateTakeOver c nx ps =
    if ps !? nx == Nothing then True
    else (isWhite c) /= (isWhite . fromJust $ ps !? nx)

validatePieceBehavior :: Char -> (Int, Int) -> (Int, Int) -> Map (Int, Int) Char -> Bool
validatePieceBehavior c og nx ps
    | c == '♜' || c == '♖' = validRook og nx
    | c == '♞' || c == '♘' = validKnight og nx
    | c == '♝' || c == '♗' = validBishop og nx
    | c == '♛' || c == '♕' = True
    | c == '♚' || c == '♔' = validKing og nx
    | c == '♟' = validWhitePawn og nx ps
    | c == '♙' = validBlackPawn og nx ps

isWhite x = elem x whitePieces
isBlack x = elem x blackPieces
whitePieces = ['♜', '♞', '♝', '♛', '♚', '♟']
blackPieces = ['♖', '♘', '♗', '♕', '♔', '♙']


piecesBetween og x y ps = piecesBetween' og (oneLess x (fst og)) (oneLess y (snd og)) ps

piecesBetween' og x y ps
    | x == fst og && y == snd og = False
    | otherwise = if ps !? (x, y) /= Nothing then True
        else piecesBetween' og (oneLess x (fst og)) (oneLess y (snd og)) ps 

oneLess :: Int -> Int -> Int
oneLess x y 
    | x == y = x
    | x < y = x + 1
    | otherwise = x - 1
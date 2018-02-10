module PieceBehavior where

validPawnUp og nx = let
    x = fst nx - fst og
    y = snd nx - snd og
    in (inBounds nx) && x == 0 && (y == 1 || y == 2)
    
validPawnDown og nx = let
    x = fst og - fst nx
    y = snd og - snd nx
    in (inBounds nx) && x == 0 && (y == 1 || y == 2)

validKnight og nx = let
    x = fromIntegral . abs $ (-) (fst og) (fst nx)
    y = fromIntegral . abs $ (-) (snd og) (snd og)
    d = abs $ (/) x y
    in (inBounds nx)
    && (d == 0.5 || d == 2)
    && (x == 2 || x == 1)
    && (y == 2 || y == 1)

validBishop og nx = let
    x = abs $ (-) (fst og) (fst nx)
    y = abs $ (-) (snd og) (snd og)
    in (inBounds nx)
    && (x == y)

validRook og nx = let
    x = abs $ (-) (fst og) (fst nx)
    y = abs $ (-) (snd og) (snd og)
    in (inBounds nx)
    && (x > 0 && y == 0)
    && (y > 0 && x == 0)
    
inBounds nx = let
    x = fst nx
    y = snd nx
    in (x <= 7) && (x >= 0)
    && (y <= 7) && (y >= 0)

validKing og nx = let
    x = abs $ (-) (fst og) (fst nx)
    y = abs $ (-) (snd og) (snd og)
    in x < 2 && y < 2 && (x /= 0 || y /= 0)

validateMove :: Char -> (Int, Int) -> (Int, Int) -> Bool
validateMove c og nx 
    | c == '♜' || c == '♖' = validRook og nx
    | c == '♞' || c == '♘' = validKnight og nx
    | c == '♝' || c == '♗' = validBishop og nx
    | c == '♛' || c == '♕' = True
    | c == '♚' || c == '♔' = validKing og nx
    | c == '♟' = validPawnUp og nx
    | c == '♙' = validPawnDown og nx

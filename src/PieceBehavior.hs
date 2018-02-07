module PieceBehavior where

pawn og nx b = 0

knight og nx = let
    x = abs $ (-) (fst og) (fst nx)
    y = abs $ (-) (snd og) (snd og)
    d = abs $ (/) x y
    in (inBounds nx)
    && (d == 0.5 || d == 2)
    && (x == 2 || x == 1)
    && (y == 2 || y == 1)

bishop og nx = let
    x = abs $ (-) (fst og) (fst nx)
    y = abs $ (-) (snd og) (snd og)
    in (inBounds nx)
    && (x == y)

rook og nx = let
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


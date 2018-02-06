module Board where

import Data.Map as M
import Prelude as P
import Control.Lens as L
board = ["◻◼◻◼◻◼◻◼"
    , "◼◻◼◻◼◻◼◻"
    , "◻◼◻◼◻◼◻◼"
    , "◼◻◼◻◼◻◼◻"
    , "◻◼◻◼◻◼◻◼"
    , "◼◻◼◻◼◻◼◻"
    , "◻◼◻◼◻◼◻◼"
    , "◼◻◼◻◼◻◼◻"
    ]


-- data ChessPieces = Map [Char] (Char, (Integer, Integer))
    
initPieces :: Map String (Char, (Int, Int))
initPieces = M.fromList [("rk1", ('♜', (0, 0)))
            , ("rk2", ('♜', (7, 0)))
            , ("rk3", ('♖', (0, 7)))
            , ("rk4", ('♖', (7, 7)))

            , ("kn1", ('♞', (1, 0)))
            , ("kn2", ('♞', (6, 0)))
            , ("kn3", ('♘', (1, 7)))
            , ("kn4", ('♘', (6, 7)))

            , ("bp1", ('♝', (2, 0)))
            , ("bp2", ('♝', (5, 0)))
            , ("bp3", ('♗', (2, 7)))
            , ("bp4", ('♗', (5, 7)))

            , ("kg1", ('♛', (3, 0)))
            , ("kg2", ('♕', (3, 7)))

            , ("qn1", ('♚', (4, 0)))
            , ("qn2", ('♔', (4, 7)))

            , ("pn1", ('♟', (0, 1)))
            , ("pn2", ('♟', (1, 1)))
            , ("pn3", ('♟', (2, 1)))
            , ("pn4", ('♟', (3, 1)))
            , ("pn5", ('♟', (4, 1)))
            , ("pn6", ('♟', (5, 1)))
            , ("pn7", ('♟', (6, 1)))
            , ("pn8", ('♟', (7, 1)))

            , ("pn9", ('♙', (0, 6)))
            , ("pn10", ('♙', (1, 6)))
            , ("pn11", ('♙', (2, 6)))
            , ("pn12", ('♙', (3, 6)))
            , ("pn13", ('♙', (4, 6)))
            , ("pn14", ('♙', (5, 6)))
            , ("pn15", ('♙', (6, 6)))
            , ("pn16", ('♙', (7, 6)))
    ]


createBoard :: [String] -> Map String (Char, (Int, Int)) -> [String] -> [String] 
createBoard b p [] = b
createBoard b p ks = createBoard (place b (p ! (head ks))) p (P.drop 1 ks)

place :: [String] -> (Char, (Int, Int)) -> [String]
place b p = let
    y = snd . snd $ p
    c = fst p
    row = (!!) b y
    x = fst . snd $ p
    newRow = (element x .~ c) row
    in 
        (element y .~ newRow) b

module Board where

import Data.Map as M
import Prelude as P
import Control.Lens as L

board :: [String]
board = ["◻◼◻◼◻◼◻◼"
    , "◼◻◼◻◼◻◼◻"
    , "◻◼◻◼◻◼◻◼"
    , "◼◻◼◻◼◻◼◻"
    , "◻◼◻◼◻◼◻◼"
    , "◼◻◼◻◼◻◼◻"
    , "◻◼◻◼◻◼◻◼"
    , "◼◻◼◻◼◻◼◻"
    ]


initPieces :: Map (Int, Int) Char
initPieces = M.fromList [((0, 7), '♜')
    , ((7, 7), '♜')
    , ((0, 0), '♖')
    , ((7, 0), '♖')
    , ((1, 7), '♞')
    , ((6, 7), '♞')
    , ((1, 0), '♘')
    , ((6, 0), '♘')
    , ((2, 7), '♝')
    , ((5, 7), '♝')
    , ((2, 0), '♗')
    , ((5, 0), '♗')
    , ((3, 7), '♛')
    , ((3, 0), '♕')
    , ((4, 7), '♚')
    , ((4, 0), '♔')
    , ((0, 6), '♟')
    , ((1, 6), '♟')
    , ((2, 6), '♟')
    , ((3, 6), '♟')
    , ((4, 6), '♟')
    , ((5, 6), '♟')
    , ((6, 6), '♟')
    , ((7, 6), '♟')
    , ((0, 1), '♙')
    , ((1, 1), '♙')
    , ((2, 1), '♙')
    , ((3, 1), '♙')
    , ((4, 1), '♙')
    , ((5, 1), '♙')
    , ((6, 1), '♙')
    , ((7, 1), '♙')
    ]


createBoard :: [String] -> Map (Int, Int) Char -> [(Int, Int)] -> [String] 
createBoard b m ks = P.foldl (\a k -> let
    y = snd k
    x = fst k
    row = a !! y
    p = m ! k
    newRow = (element x .~ p) row
        in (element y .~ newRow) a
    ) b ks

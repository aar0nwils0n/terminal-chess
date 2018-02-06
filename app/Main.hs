module Main where

import Lib
import Board
import Data.Map.Lazy

main :: IO ()
main = putStrLn $ Prelude.foldl (\x y -> x ++ "\n" ++ y)  "" (createBoard board initPieces (keys initPieces))

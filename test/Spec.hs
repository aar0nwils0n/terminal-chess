import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import PieceBehavior 
import Data.Map (fromList)
import Board
import GamePlay

main :: IO ()
main = hspec $ do

    describe "PieceBehavior.validBlackPawn" $ do
        it "should be able to take over a black peice diagonally" $ do
            let pieces = fromList [((0, 0), '♟'), ((1, 1), '♙')]
                in validBlackPawn (0, 0) (1, 1) pieces `shouldBe` True
        it "should not be able to go diagonally otherwise" $ do
            let pieces = fromList [((0, 0), '♟'), ((0, 2), '♙')]
                in validBlackPawn (0, 0) (1, 1) pieces `shouldBe` False
        it "should be able to go forward 2 paces from the get go" $ do
            validBlackPawn (0, 1) (0, 3) initPieces `shouldBe` True
        it "should not be able to go forward 2 paces otherwise" $ do
            validBlackPawn (0, 2) (0, 4) initPieces `shouldBe` False

    describe "PieceBehavior.validWhitePawn" $ do 
        it "should be able to take over a black peice diagonally" $ do
            let pieces = fromList [((0, 0), '♙'), ((1, 1), '♟')]
                in validWhitePawn (1, 1) (0, 0) pieces `shouldBe` False
        it "should not be able to go diagonally otherwise" $ do
            let pieces = fromList [((0, 0), '♙')]
                in validWhitePawn (1, 1) (0, 0) pieces `shouldBe` False
        it "should be able to go forward 2 paces from the get go" $ do
            validWhitePawn (0, 6) (0, 4) initPieces `shouldBe` True
        it "should not be able to go forward 2 paces otherwise" $ do
            validWhitePawn (0, 5) (0, 3) initPieces `shouldBe` False

    describe "PieceBehavior.oneLess" $ do 
        it "should return one step closer to x if y is greater" $ do
            oneLess 0 4 `shouldBe` 1
        it "should return one step closer to x if x is greater" $ do
            oneLess 4 1 `shouldBe` 3
        it "should do nothing if x and y are the same" $ do
            oneLess 1 1 `shouldBe` 1

    describe "PieceBehavior.piecesBetween" $ do 
        it "should return True if there are pieces in between" $ do
            let ps = fromList [((1, 1), 'a')]
                in piecesBetween (0, 0) 2 2 ps `shouldBe` True
        it "should return False if there are not pieces in between" $ do 
            piecesBetween (3,3) 1 1 initPieces `shouldBe` False

    describe "PieceBehavior.validRook" $ do 
        it "should be able to move forward a space" $ do
            let ps = fromList [((0, 0), '♜')]
                in validRook (0, 0) (0, 1) `shouldBe` True
 
    describe "Gameplay.letterToNumber" $ do 
        it "should convert a to 0" $ do
            letterToNumber 'a' `shouldBe` 0
        it "should convert h to 8" $ do
            letterToNumber 'h' `shouldBe` 7

    describe "Gameplay.convertCoord" $ do
        it "should convert 8 to 0" $ do
            convertCoord 8 `shouldBe` 0
        it "should convert 8 to 0" $ do
            convertCoord 1 `shouldBe` 7


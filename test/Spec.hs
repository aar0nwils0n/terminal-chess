import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import PieceBehavior 
import Data.Map (fromList)
import Board

main :: IO ()
main = hspec $ do

    describe "PieceBehavior.validWhitePawn" $ do
        it "should be able to take over a black peice diagonally" $ do
            let pieces = fromList [((0, 0), '♟'), ((1, 1), '♙')]
                in validWhitePawn (0, 0) (1, 1) pieces `shouldBe` True
        it "should not be able to go diagonally otherwise" $ do
            let pieces = fromList [((0, 0), '♟'), ((0, 2), '♙')]
                in validWhitePawn (0, 0) (1, 1) pieces `shouldBe` False
        it "should be able to go forward 2 paces from the get go" $ do
            validWhitePawn (0, 1) (0, 3) initPieces `shouldBe` True
        it "should not be able to go forward 2 paces otherwise" $ do
            validWhitePawn (0, 2) (0, 4) initPieces `shouldBe` False

    describe "PieceBehavior.validBlackPawn" $ do 
        it "should be able to take over a black peice diagonally" $ do
            let pieces = fromList [((0, 0), '♙'), ((1, 1), '♟')]
                in validBlackPawn (1, 1) (0, 0) pieces `shouldBe` False
        it "should not be able to go diagonally otherwise" $ do
            let pieces = fromList [((0, 0), '♙')]
                in validBlackPawn (1, 1) (0, 0) pieces `shouldBe` False
        it "should be able to go forward 2 paces from the get go" $ do
            validBlackPawn (0, 6) (0, 4) initPieces `shouldBe` True
        it "should not be able to go forward 2 paces otherwise" $ do
            validBlackPawn (0, 5) (0, 3) initPieces `shouldBe` False

    describe "PieceBehavior.piecesBetween" $ do 
        it "should return True if there are pieces in between" $ do
            let ps = fromList [((1, 1), 'a')]
                in piecesBetween (0, 0) 2 2 ps `shouldBe` True
        it "should return False if there are not pieces in between" $ do 
            piecesBetween (3,3) 2 2 initPieces `shouldBe` True

    describe "PieceBehavior.oneLess" $ do 
        it "should return one step closer to x if y is greater" $ do
            oneLess 0 4 `shouldBe` 3
        it "should return one step closer to x if x is greater" $ do
            oneLess 4 0 `shouldBe` 1 
 
import Test.Hspec
import Control.Exception (evaluate)
import Lib

main :: IO ()
main = hspec $ do
  describe "listLength" $ do
    it "returns 0 for []" $
      listLength [] `shouldBe` 0

    it "returns 1 for [1]" $
      listLength [1] `shouldBe` 1

    it "returns 5 for [5..9]" $
      listLength [5..9] `shouldBe` 5

  describe "meanList" $ do
    it "throws error for empty" $
      evaluate (meanList []) `shouldThrow` errorCall "No items"

    it "returns same value for 1 item" $
      meanList [1] `shouldBe` 1

    it "returns mid value for 2 ints" $
      meanList [1,3] `shouldBe` 2

    it "returns fractional mean value for 4 ints" $
      meanList [1,2,3,4] `shouldBe` 2.5

  describe "repeatToPalindrome" $ do
    it "should return empty for empty list" $
      repeatToPalindrome [] `shouldBe` []

    it "should return 2 length for single list" $
      repeatToPalindrome [1] `shouldBe` [1,1]

    it "should return longer length reversed for multiple list" $
      repeatToPalindrome [2,3,4] `shouldBe` [2,3,4,4,3,2]

  describe "isPalindrome" $ do
    it "should return true for []" $
      isPalindrome [] `shouldBe` True

    it "should return false for single [1]" $
      isPalindrome [1] `shouldBe` False

    it "should return true for mirrored" $
      isPalindrome [1,2,3,3,2,1] `shouldBe` True

  describe "sortListsByLength" $ do
    it "should return [] for []" $
      sortListsByLength [] `shouldBe` []

    it "should return [[1]] for [[1]]" $
      sortListsByLength [[1]] `shouldBe` [[1]]

    it "should return [[1..2], [1..5], [1..10]] for [[1..10], [1..5], [1..2]]" $
      sortListsByLength [[1..10], [1..5], [1..2]] `shouldBe` [[1..2], [1..5], [1..10]]

  describe "myIntersperse" $ do
    it "should throw error for []" $
      evaluate (myIntersperse ',' []) `shouldThrow` errorCall "No items"

    it "should return correct for single entry" $
      myIntersperse ',' ["hello"] `shouldBe` "hello"

    it "should return correct for multiple entries" $
      myIntersperse ',' ["foo","bar","baz","quux"] `shouldBe` "foo,bar,baz,quux"

  describe "treeHeight" $ do
    it "should return 0 for Empty" $
      treeHeight Empty `shouldBe` 0

    it "should return 1 for single node" $
      treeHeight (Node 'a' Empty Empty) `shouldBe` 1

    it "should return 3 for deep tree" $
      treeHeight (Node 'a' (Node 'b' Empty (Node 'c' Empty Empty)) Empty) `shouldBe` 3

    it "should return 3 for dense tree" $
      treeHeight (Node 'a' (Node 'b' (Node 'c' Empty Empty) (Node 'd' Empty Empty)) (Node 'e' Empty Empty)) `shouldBe` 3

  describe "turnDirection" $ do
    it "should return straight for inline" $
      turnDirection (Point2 1 1) (Point2 1 3) (Point2 1 4) `shouldBe` Straight

    it "should return right for right turn" $
      turnDirection (Point2 1 1) (Point2 1 3) (Point2 3 3) `shouldBe` RightTurn

    it "should return left for left turn" $
      turnDirection (Point2 1 1) (Point2 1 (-1)) (Point2 3 (-1)) `shouldBe` LeftTurn

  describe "turnDirections" $ do
    it "should return correct sequence for 3" $
      turnDirections [(Point2 1 1), (Point2 1 3), (Point2 1 4), (Point2 2 4), (Point2 2 6)] `shouldBe` [Straight, RightTurn, LeftTurn]

  describe "grahamScanStartingPoint" $ do
    it "should error for empty" $
      evaluate (grahamScanStartingPoint []) `shouldThrow` errorCall "No items"

    it "should return result for single" $
      grahamScanStartingPoint [(Point2 1 1)] `shouldBe` (Point2 1 1)

    it "should return result for multiple" $
      grahamScanStartingPoint [(Point2 1 1), (Point2 10 0), (Point2 5 5)] `shouldBe` (Point2 10 0)

    it "should return lowest x for tie breaking" $
      grahamScanStartingPoint [(Point2 10 0), (Point2 1 0), (Point2 5 0)] `shouldBe` (Point2 1 0)

  describe "grahamScan" $ do
    it "should return empty for empty" $
      grahamScan [] `shouldBe` []

    it "should return single for single" $
      grahamScan [(Point2 1 1)] `shouldBe` [(Point2 1 1)]

    it "should return double for double" $
      grahamScan [(Point2 1 1), (Point2 2 2)] `shouldBe` [(Point2 1 1), (Point2 2 2)]

    it "should return input for triangle" $
      grahamScan [(Point2 0 0), (Point2 1 1), (Point2 2 0)] `shouldMatchList` [(Point2 0 0), (Point2 1 1), (Point2 2 0)]

    it "should return correct for larger for triangle" $
      grahamScan [(Point2 0 0), (Point2 2 2), (Point2 4 0), (Point2 1 1)] `shouldMatchList` [(Point2 0 0), (Point2 2 2), (Point2 4 0)]
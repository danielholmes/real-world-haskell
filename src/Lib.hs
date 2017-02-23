module Lib (
    listLength,
    meanList,
    repeatToPalindrome,
    isPalindrome,
    sortListsByLength,
    myIntersperse,
    Tree (Empty, Node),
    treeHeight,
    Direction (LeftTurn, RightTurn, Straight),
    Point2 (Point2),
    turnDirection,
    turnDirections,
    grahamScanStartingPoint,
    sortByAngleTo,
    angleBetween,
    grahamScan
) where

import Data.List
import Data.Function
--import Debug.Trace

listLength :: [a] -> Integer
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

meanList :: [Integer] -> Double
meanList [] = error "No items"
meanList [x] = fromIntegral x
meanList x = fromIntegral (sum x) / fromIntegral (length x)

-- TODO: Change to generic (a instead of Integer)
repeatToPalindrome :: [Integer] -> [Integer]
repeatToPalindrome [] = []
repeatToPalindrome (x:xs) = x : repeatToPalindrome xs ++ [x]

-- TODO: Change to a or Eq
isPalindrome :: [Integer] -> Bool
isPalindrome [] = True
isPalindrome x
    | length x `mod` 2 == 1 = False
    | otherwise = repeatToPalindrome firstHalf == x
        where
            halfLength = (length x) `div` 2
            firstHalf = take halfLength x

-- TODO: Change to a or Eq
sortListsByLength :: [[Integer]] -> [[Integer]]
sortListsByLength [] = []
sortListsByLength x = sortBy (compare `on` length) x

-- TODO: Change to a or Eq
myIntersperse :: Char -> [[Char]] -> [Char]
myIntersperse _ [] = error "No items"
myIntersperse _ (x:[]) = x
myIntersperse c (x:xs) = x ++ [c] ++ myIntersperse c xs

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

treeHeight :: Tree a -> Integer
treeHeight Empty = 0
treeHeight (Node _ l r) = maximum [(1 + treeHeight l), (1 + treeHeight r)]

data Direction = LeftTurn | RightTurn | Straight deriving (Show, Eq)
data Point2 = Point2 { pointX :: Integer, pointY :: Integer } deriving (Show, Eq)

lineAngle :: Point2 -> Point2 -> Double
lineAngle (Point2 ax ay) (Point2 bx by) = atan2 (fromIntegral (by - ay)) (fromIntegral (bx - ax))

angleBetween :: Point2 -> Point2 -> Point2 -> Double
angleBetween a b c
    | angleDiff > pi = angleDiff - (2 * pi)
    | otherwise = angleDiff
    where
        angle1 = lineAngle a b
        angle2 = lineAngle b c
        angleDiff = angle1 - angle2

turnDirection :: Point2 -> Point2 -> Point2 -> Direction
turnDirection a b c
    | angleDiff > 0 = RightTurn
    | angleDiff < 0 = LeftTurn
    | otherwise = Straight
    where angleDiff = angleBetween a b c

turnDirections :: [Point2] -> [Direction]
turnDirections [] = []
turnDirections [_] = []
turnDirections [_,_] = []
turnDirections (a:ps@(b:c:_)) = turnDirection a b c : turnDirections ps

-- note: see wikipedia article on graham scan - more efficient method than getting angle
sortByAngleTo :: Point2 -> [Point2] -> [Point2]
sortByAngleTo p ps = sortBy (compare `on` (\x -> if p == x then -9999999 else (angleBetween xAxis p x))) ps
    where xAxis = movePoint p 1 0

grahamScanStartingPoint :: [Point2] -> Point2
grahamScanStartingPoint [] = error "No items"
grahamScanStartingPoint ps = minimumBy (compare `on` (\p -> ((-(pointY p)),(-(pointX p))))) ps

grahamScan :: [Point2] -> [Point2]
grahamScan ps
    | (length ps) < 3 = error "Need at least 3"
    | otherwise = grahamScanStep (reverse (take 3 sorted)) (drop 3 sorted)
    where
        p = grahamScanStartingPoint ps
        sorted = sortByAngleTo p ps

grahamScanStep :: [Point2] -> [Point2] -> [Point2]
grahamScanStep x [] = x
grahamScanStep [] _ = error "Should always be atleast 3 inside (0)"
grahamScanStep [_] _ = error "Should always be atleast 3 inside (1)"
grahamScanStep [_,_] _ = error "Should always be atleast 3 inside (2)"
grahamScanStep inside@(x:xs@(y:_)) (p:ps) =
    case (turnDirection y x p) of LeftTurn -> grahamScanStep (p:xs) ps
                                  _ -> grahamScanStep (p : inside) ps

movePoint :: Point2 -> Integer -> Integer -> Point2
movePoint p 0 0 = p
movePoint p x y = Point2 ((pointX p) + x) ((pointY p) + y)
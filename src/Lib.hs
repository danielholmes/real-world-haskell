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
    grahamScan
) where

import Data.List
import Data.Function

listLength :: [a] -> Integer
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

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
treeHeight (Node a l r) = maximum [(1 + treeHeight l), (1 + treeHeight r)]

data Direction = LeftTurn | RightTurn | Straight deriving (Show, Eq)
data Point2 = Point2 { pointX :: Integer, pointY :: Integer } deriving (Show, Eq)

angleBetween :: Point2 -> Point2 -> Point2 -> Double
angleBetween (Point2 ax ay) (Point2 bx by) (Point2 cx cy) = angle1 - angle2
    where
        angle1 = atan2 (fromIntegral (bx - ax)) (fromIntegral (by - ay))
        angle2 = atan2 (fromIntegral (cx - bx)) (fromIntegral (cy - by))

turnDirection :: Point2 -> Point2 -> Point2 -> Direction
turnDirection a b c
    | angleDiff < 0 = RightTurn
    | angleDiff > 0 = LeftTurn
    | otherwise = Straight
    where angleDiff = angleBetween a b c

turnDirections :: [Point2] -> [Direction]
turnDirections [] = []
turnDirections [_] = []
turnDirections [_,_] = []
turnDirections (a:ps@(b:c:_)) = turnDirection a b c : turnDirections ps

-- note: see wikipedia article on graham scan - more efficient method than getting angle
sortByAngleWithPointAndXAxis :: Point2 -> [Point2] -> [Point2]
sortByAngleWithPointAndXAxis p ps = sortBy (compare `on` (\x -> angleBetween x p (movePoint x 1 0))) ps

grahamScanStartingPoint :: [Point2] -> Point2
grahamScanStartingPoint [] = error "No items"
grahamScanStartingPoint ps = minimumBy (compare `on` (\p -> ((pointY p),(pointX p)))) ps

grahamScan :: [Point2] -> [Point2]
grahamScan [] = []
grahamScan [a] = [a]
grahamScan [a,b] = [a,b]
grahamScan [a,b,c] = [a,b,c]
grahamScan ps = grahamScanStep sorted ++ (take 2 sorted)
    where
        p = grahamScanStartingPoint ps
        sorted = sortByAngleWithPointAndXAxis p ps

grahamScanStep :: [Point2] -> [Point2]
grahamScanStep [] = []
grahamScanStep [_] = []
grahamScanStep [_,_] = []
grahamScanStep (a:ps@(b:c:_)) =
    case (turnDirection a b c) of RightTurn -> grahamScanStep ps
                                  _ -> a : grahamScanStep ps

{-

a b c (d:ds)
t = turnDirection a b c
if t == LEFT || STRAIGHT
    a : step b c d ds
else (RIGHT - a is inside)
    step b c d ds


-}
{-
 1. considering each of the points in the sorted array in sequence
   a. It is first determined whether traveling from the two points immediately preceding this point constitutes making a left turn or a right turn
     i.  If a right turn, the second-to-last point is not part of the convex hull, and lies 'inside' it.
     ii. The same determination is then made for the set of the latest point and the two points that immediately precede
         the point found to have been inside the hull, and is repeated until a "left turn" set is encountered, at which
         point the algorithm moves on to the next point in the set of points in the sorted array minus any points that
         were found to be inside the hull; there is no need to consider these points again. (If at any stage the three
         points are collinear, one may opt either to discard or to report it, since in some applications it is required
         to find all points on the boundary of the convex hull.)
-}

movePoint :: Point2 -> Integer -> Integer -> Point2
movePoint p 0 0 = p
movePoint p x y = Point2 ((pointX p) + x) ((pointY p) + y)
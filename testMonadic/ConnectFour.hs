module ConnectFour (gameName, R, Move, p, epsilons) where

import qualified Data.List as L
import InfSupSelections
import J

gameName = "ConnectFour"

type R = Int
type Move = Int
type Board = [[Player]]
data Player = X | O
  deriving (Eq)

wins :: Board -> Player -> Bool
wins b p = checkWin 0 0 b p
  where
    checkWin :: Int -> Int -> Board -> Player -> Bool
    checkWin 0 6 _ _ = False
    checkWin 6 y b p = (b ! (6, y) == Just p) && ((row 6 y b p) || (colum 6 y b p)
                       || (left 6 y b p) || (right 6 y b p)) || (checkWin 0 (y + 1) b p)
    checkWin x y b p = (b ! (x,y) == Just p) && ((row x y b p) || (colum x y b p)
                       || (left x y b p) || (right x y b p)) || (checkWin (x + 1) y b p)
    row x y b p   = b ! (x, y) == Just p && b ! (x+1, y) == Just p && b ! (x+2, y) == Just p && b ! (x+3, y) == Just p
    colum x y b p = b ! (x, y) == Just p && b ! (x, y+1) == Just p && b ! (x, y+2) == Just p && b ! (x, y+3) == Just p
    left x y b p  = b ! (x, y) == Just p && b ! (x+1, y+1) == Just p && b ! (x+2, y+2) == Just p && b ! (x+3, y+3) == Just p
    right x y b p = b ! (x, y) == Just p && b ! (x-1, y+1) == Just p && b ! (x-2, y+2) == Just p && b ! (x-3, y+3) == Just p
    (!) :: [[a]] -> (Int, Int) -> Maybe a
    x ! (i, j) = if i >= 0 && j >= 0 &&  length x > i && length (x !! i) > j then  Just $ (x !! i) !! j else Nothing

value :: Board -> R
value b  | wins b X  =  1
         | wins b O  = -1
         | otherwise =  0

outcome :: Player -> [Move] -> Board -> Board
outcome _ [] b   = b
outcome p (m : ms) b = if wins b p then b else outcome (changePlayer p) ms (insert m p b)

changePlayer :: Player -> Player
changePlayer X = O
changePlayer O = X

p :: [Move] -> R
p ms = value(outcome X ms [[]])

insert :: Move -> Player -> Board -> Board
insert 1 p (x:xs) = (x ++ [p]) : xs
insert 1 p []     = [[p]]
insert m p (x:xs) = x : (insert (m - 1) p xs)
insert m p []     = [] : (insert (m - 1) p [])

getPossibleMoves :: [Move] -> [Move]
getPossibleMoves [] = [1..7]
getPossibleMoves xs = filter (\x -> (length $ L.elemIndices x xs) < 6) [1..7]

epsilons :: [[Move] -> J R Move]
epsilons = take 42 all
  where all = epsilonX : epsilonO : all
        epsilonX history = epsilonSupThree (getPossibleMoves history)
        epsilonO history = epsilonInfThree (getPossibleMoves history)

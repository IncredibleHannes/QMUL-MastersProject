{- |
   Module     : ConnectFour
   Copyright  : Copyright (C) 2018 Johannes Hartmann
   License    : MIT
   Maintainer : Johannes Hartmann <Johannes.Hartmann.Calw@web.de>
   Example Connect four implementation. This version does not have any performance
   optimisations
   Written by Johannes Hartmann, Johannes.Hartmann.Calw@web.de
-}
import qualified Data.List      as L
import           Data.Selection

gameName = "ConnectFour"

type R = Int
type Move = Int
type Board = [[Player]]
data Player = X | O
  deriving (Eq)

wins :: Board -> Player -> Bool
wins = checkWin 0 0
  where
    checkWin :: Int -> Int -> Board -> Player -> Bool
    checkWin 0 6 _ _ = False
    checkWin 6 y b p = (b ! (6, y) == Just p) && (row 6 y b p || colum 6 y b p
                       || left 6 y b p || right 6 y b p) || checkWin 0 (y + 1) b p
    checkWin x y b p = (b ! (x,y) == Just p) && (row x y b p || colum x y b p
                       || left x y b p || right x y b p) || checkWin (x + 1) y b p
    row x y b p   = b ! (x, y) == Just p && b ! (x+1, y) == Just p && b ! (x+2, y) == Just p && b ! (x+3, y) == Just p
    colum x y b p = b ! (x, y) == Just p && b ! (x, y+1) == Just p && b ! (x, y+2) == Just p && b ! (x, y+3) == Just p
    left x y b p  = b ! (x, y) == Just p && b ! (x+1, y+1) == Just p && b ! (x+2, y+2) == Just p && b ! (x+3, y+3) == Just p
    right x y b p = b ! (x, y) == Just p && b ! (x-1, y+1) == Just p && b ! (x-2, y+2) == Just p && b ! (x-3, y+3) == Just p
    (!) :: [[a]] -> (Int, Int) -> Maybe a
    (!) x (i, j) = if i >= 0 && j >= 0 &&  length x > i && length (x !! i) > j then  Just $ (x !! i) !! j else Nothing

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

insert :: Move -> Player -> Board -> Board
insert 1 p (x:xs) = (x ++ [p]) : xs
insert 1 p []     = [[p]]
insert m p (x:xs) = x : insert (m - 1) p xs
insert m p []     = [] : insert (m - 1) p []

getPossibleMoves :: [Move] -> [Move]
getPossibleMoves [] = [1..7]
getPossibleMoves xs = filter (\x -> length (L.elemIndices x xs) < 6) [1..7]

p :: [Move] -> R
p ms = value(outcome X ms [[]])

epsilons :: [[Move] -> J R Move]
epsilons = take 5 all
  where all = epsilonX : epsilonO : all
        epsilonX history = epsilonMaxParalell (getPossibleMoves history)
        epsilonO history = epsilonMinParalell (getPossibleMoves history)

main :: IO ()
main = do
  let optimalGame = optimalPlay p epsilons
  putStr ("An optimal play for " ++ gameName ++ " is "
     ++ show optimalGame
     ++ "\nand the optimal outcome is " ++ show (p optimalGame) ++ "\n")

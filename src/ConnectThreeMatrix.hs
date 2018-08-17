{- |
   Module     : ConnectThreeMatrixTuple
   Copyright  : Copyright (C) 2018 Johannes Hartmann
   License    : MIT
   Maintainer : Johannes Hartmann <Johannes.Hartmann.Calw@web.de>
   Example Connect four implementation.
   The wins function is optimised saving some computations, and the Matrix
   library is used to support O(1) acces times.

   The output of the computation is the following:

   An optimal play for ConnectThreeMatrix is [4,1,4,1,4,1,5,2,5]
   and the optimal outcome is 1
   (1959.94 secs, 590,844,013,624 bytes)

   Written by Johannes Hartmann, Johannes.Hartmann.Calw@web.de
-}

module ConnectThreeMatrix (gameName, R, Move, p, epsilons) where

import qualified Data.List      as L
import           Data.Matrix
import           Data.Selection

gameName = "ConnectThreeMatrix"

type R = Int
type Move = Int
type Board = Matrix Player
data Player = X | O | N
  deriving (Eq, Show)

wins :: Board -> Player -> Bool
wins = checkWin 1 1
  where
    checkWin :: Int -> Int -> Board -> Player -> Bool
    checkWin 4 3 _ _ = False
    checkWin 5 y b p = ((b ! (5, y) == p) && (row 5 y b p || colum 5 y b p || left 5 y b p || right 5 y b p)) || checkWin 1 (y + 1) b p
    checkWin x y b p = ((b ! (x, y) == p) && (row x y b p || colum x y b p || left x y b p || right x y b p)) || checkWin (x + 1) y b p
    row x y b p      = x < 4 && (b ! (x, y) == p && b ! (x+1, y) == p && b ! (x+2, y) == p)
    colum x y b p    = y == 1 && (b ! (x, y) == p && b ! (x, y+1) == p && b ! (x, y+2) == p)
    left x y b p     = y == 1 && x < 4 && (b ! (x, y) == p && b ! (x+1, y+1) == p && b ! (x+2, y+2) == p)
    right x y b p    = y == 1 && x > 2 && (b ! (x, y) == p && b ! (x-1, y+1) == p && b ! (x-2, y+2) == p)

value :: Board -> R
value b  | wins b X  = 1
         | wins b O  = -1
         | otherwise = 0

outcome :: Player -> [Move] -> Board -> Board
outcome _ [] b   = b
outcome p (m : ms) b = let nb = insert m p b in
                       if wins nb p then nb else outcome (changePlayer p) ms nb

changePlayer :: Player -> Player
changePlayer X = O
changePlayer O = X

insert :: Move -> Player -> Board -> Board
insert m = insert' (m, 1)
  where insert' (x, y) p b = if b ! (x, y) == N
                             then setElem p (x, y) b
                             else insert' (x, y+1) p b

getPossibleMoves :: [Move] -> [Move]
getPossibleMoves [] = [1..5]
getPossibleMoves xs = filter (\x -> length (L.elemIndices x xs) < 3) [1..5]

p :: [Move] -> R
p ms = value(outcome X ms (matrix 5 3 (const N)))

epsilons :: [[Move] -> J R Move]
epsilons = take 7 all
  where all = epsilonX : epsilonO : all
        epsilonX history = epsilonMaxThree (getPossibleMoves history)
        epsilonO history = epsilonMinThree (getPossibleMoves history)

main :: IO ()
main = do
  let optimalGame = optimalPlay p epsilons
  putStr ("An optimal play for " ++ gameName ++ " is "
     ++ show optimalGame
     ++ "\nand the optimal outcome is " ++ show (p optimalGame) ++ "\n")

{-
  An optimal play for ConnectThreeMatrix is [4,1,4,1,4,1,5,2,5]
  and the optimal outcome is 1
  (1959.94 secs, 590,844,013,624 bytes)
-}

module ConnectThreeMatrixTuple (gameName, R, Move, p, epsilons) where

import qualified Data.List as L
import InfSupSelections
import J
import Data.Matrix

gameName = "ConnectThreeMatrixTuple"

type R = (Int,Int)
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

value :: Board -> Int
value b  | wins b X  = 1
         | wins b O  = -1
         | otherwise = 0

outcome :: Player -> [Move] -> Board -> Int -> (Board,Int)
outcome _ [] b i       = (b, i)
outcome p (m : ms) b i = let nb = insert m p b in
                       if wins nb p then (nb, i+1) else outcome (changePlayer p) ms nb (i+1)

changePlayer :: Player -> Player
changePlayer X = O
changePlayer O = X

p :: [Move] -> R
p ms = let o = outcome X ms (matrix 5 3 (const N)) 0 in (value $ fst o, snd o)

insert :: Move -> Player -> Board -> Board
insert m = insert' (m,1)
  where
    insert' (x,y) p b = if b ! (x,y) == N
                        then setElem p (x,y) b
                        else insert' (x,y+1) p b

getPossibleMoves :: [Move] -> [Move]
getPossibleMoves [] = [1..5]
getPossibleMoves xs = filter (\x -> length (L.elemIndices x xs) < 3) [1..5]

epsilons :: [[Move] -> J R Move]
epsilons = take 9 all
  where all = epsilonX : epsilonO : all
        epsilonX history = epsilonSupTuple (getPossibleMoves history)
        epsilonO history = epsilonInfTuple (getPossibleMoves history)

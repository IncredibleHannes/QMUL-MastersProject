{- |
   Module     : ConnectFourMatrixTuple
   Copyright  : Copyright (C) 2018 Johannes Hartmann
   License    : MIT
   Maintainer : Johannes Hartmann <Johannes.Hartmann.Calw@web.de>
   Example Connect four implementation. This version uses tuples as outcome type.
   The wins function is optimised saving some computations, and the Matrix
   library is used to support O(1) acces times. It also supports paralellism. To
   run it parallel compile it with the following command:

   >  ghc -O2 -threaded --make ConnectFourMatrixTuple.hs

   The computation time depending of the count of moves that are explored:

   ________|__1__|__2__|__3__|__4__|__5__|__6__|__7__|__8__|__9__|
   time    | 1.91| 1.85| 0.12| 0.13| 0.18| 0.47| 3.09| 27.0|236.9|

   Written by Johannes Hartmann, Johannes.Hartmann.Calw@web.de
-}

import qualified Data.List      as L
import           Data.Matrix
import           Data.Selection

gameName = "ConnectFourMatrixTuple"

type R = (Int,Int)
type Move = Int
type Board = Matrix Player
data Player = X | O | N
  deriving (Eq, Show)

wins :: Board -> Player -> Bool
wins = checkWin 1 1
  where
    checkWin :: Int -> Int -> Board -> Player -> Bool
    checkWin 7 6 _ _ = False
    checkWin 7 y b p = ((b ! (7, y) == p) && (row 7 y b p || colum 7 y b p || left 7 y b p || right 7 y b p)) || checkWin 1 (y + 1) b p
    checkWin x y b p = ((b ! (x, y) == p) && (row x y b p || colum x y b p || left x y b p || right x y b p)) || checkWin (x + 1) y b p
    row x y b p      = x < 5 && (b ! (x, y) == p && b ! (x+1, y) == p && b ! (x+2, y) == p && b ! (x+3, y) == p)
    colum x y b p    = y < 4 && (b ! (x, y) == p && b ! (x, y+1) == p && b ! (x, y+2) == p && b ! (x, y+3) == p)
    left x y b p     = y < 4 && x < 5 && (b ! (x, y) == p && b ! (x+1, y+1) == p && b ! (x+2, y+2) == p && b ! (x+3, y+3) == p)
    right x y b p    = y < 4 && x > 3 && (b ! (x, y) == p && b ! (x-1, y+1) == p && b ! (x-2, y+2) == p && b ! (x-3, y+3) == p)

value :: Board -> Int
value b  | wins b X  = 1
         | wins b O  = -1
         | otherwise = 0

changePlayer :: Player -> Player
changePlayer X = O
changePlayer O = X

insert :: Move -> Player -> Board -> Board
insert m = insert' (m,1)
  where
    insert' (x,y) p b = if b ! (x,y) == N
                        then setElem p (x,y) b
                        else insert' (x,y+1) p b

getPossibleMoves :: [Move] -> [Move]
getPossibleMoves [] = [1..7]
getPossibleMoves xs = filter (\x -> length (L.elemIndices x xs) < 6) [1..7]

p :: [Move] -> R
p ms = outcome X ms (matrix 7 6 (const N)) 0

outcome :: Player -> [Move] -> Board -> Int -> R
outcome _ [] b i       = (value b, i)
outcome p (m : ms) b i = let nb = insert m p b in
                         if wins nb p then (value nb, i+1) else outcome (changePlayer p) ms nb (i+1)


epsilons :: [[Move] -> J R Move]
epsilons = take 5 all
  where all = epsilonX : epsilonO : all
        epsilonX history = epsilonMaxTupleParalell (getPossibleMoves history)
        epsilonO history = epsilonMinTupleParalell (getPossibleMoves history)

main :: IO ()
main = do
  let optimalGame = optimalPlay p epsilons
  putStr ("An optimal play for " ++ gameName ++ " is "
     ++ show optimalGame
     ++ "\nand the optimal outcome is " ++ show (p optimalGame) ++ "\n")

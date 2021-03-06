{- |
   Module     : ConnectThreeMatrixTuple
   Copyright  : Copyright (C) 2018 Johannes Hartmann
   License    : MIT
   Maintainer : Johannes Hartmann <Johannes.Hartmann.Calw@web.de>
   Example Connect Three implementation. This version uses tuples as outcome type.
   The wins function is optimised saving some computations, and the Matrix
   library is used to support O(1) acces times. It also supports paralellism. To
   run it parallel compile it with the following command:

   >  ghc -O2 -threaded --make ConnectThreeMatrixTuple.hs

   The outcome of this function is the following:

   An optimal play for ConnectThreeMatrixTuple is [2,4,3,1,2,2,3,5,1]
   and the optimal outcome is (1,9)
   (1834.56 secs, 515,446,317,680 bytes)


   Time paralell: 22.855s

   Written by Johannes Hartmann, Johannes.Hartmann.Calw@web.de
-}

import qualified Data.List      as L
import           Data.Matrix
import           Data.Selection

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
getPossibleMoves [] = [1..5]
getPossibleMoves xs = filter (\x -> length (L.elemIndices x xs) < 3) [1..5]

p :: [Move] -> R
p ms = outcome X ms (matrix 5 3 (const N)) 0

outcome :: Player -> [Move] -> Board -> Int -> R
outcome _ [] b i       = (value b, i)
outcome p (m : ms) b i = let nb = insert m p b in
                         if wins nb p then (value nb, i+1) else outcome (changePlayer p) ms nb (i+1)


epsilons :: [[Move] -> J R Move]
epsilons = take 6 all
  where all = epsilonX : epsilonO : all
        epsilonX history = epsilonMaxTupleParalell (getPossibleMoves history)
        epsilonO history = epsilonMinTupleParalell (getPossibleMoves history)

main :: IO ()
main = do
  let optimalGame = optimalPlay p epsilons
  putStr ("An optimal play for " ++ gameName ++ " is "
     ++ show optimalGame
     ++ "\nand the optimal outcome is " ++ show (p optimalGame) ++ "\n")

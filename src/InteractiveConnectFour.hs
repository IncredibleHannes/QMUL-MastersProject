{- |
   Module     : InteractiveConnectFour
   Copyright  : Copyright (C) 2018 Johannes Hartmann
   License    : MIT
   Maintainer : Johannes Hartmann <Johannes.Hartmann.Calw@web.de>
   Example Interactive Connect Four implementation. In this version a human Player
   can play against the computer. This version uses tuples as outcome type.
   The wins function is optimised saving some computations, and the Matrix
   library is used to support O(1) acces times. It also supports paralellism. To
   run it parallel compile it with the following command:

   >  ghc -O2 -threaded --make InteractiveConnectFour.hs

   Written by Johannes Hartmann, Johannes.Hartmann.Calw@web.de
-}

import qualified Data.List      as L
import           Data.Matrix
import           Data.Selection

gameName = "InteractiveConnectFour"

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

outcome :: Player -> [Move] -> Board -> Int -> (Board,Int)
outcome _ [] b i       = (b, i)
outcome p (m : ms) b i = let nb = insert m p b in
                        if wins nb p then (nb, i+1) else outcome (changePlayer p) ms nb (i+1)
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
p ms = let o = outcome X ms (matrix 7 6 (const N)) 0 in (value $ fst o, snd o)

epsilons :: [[Move] -> J R Move]
epsilons = epsilons' []

epsilons' :: [Move] -> [[Move] -> J R Move]
epsilons' h = take (if length h > 35 then 42 - length h else 6) all
  where all = epsilonO : epsilonX : all
        epsilonX history = epsilonMaxTupleParalell (getPossibleMoves (h ++ history))
        epsilonO history = epsilonMinTupleParalell (getPossibleMoves (h ++ history))

printBoard :: Board -> IO ()
printBoard b = print $ fromLists $ reverse $ toLists (transpose b)

isGameOver :: Board -> Bool
isGameOver b = (value b /= 0) || (N `notElem` toList b)

gameRound :: [Move] -> IO ()
gameRound history = do
  putStrLn "Please select your move (1-7): "
  x <- readLn
  if x `notElem` getPossibleMoves history then gameRound history else do
    let aiMove = optimalStrategy p (epsilons' (history ++ [x])) (history ++ [x])
    let newBoard = outcome X (history ++ [x] ++ [aiMove]) (matrix 7 6 (const N)) 0
    if isGameOver $ fst newBoard
      then do
        printBoard $ fst newBoard
        putStrLn "The game is over. Thank You for playing!"
      else do
        putStrLn $ "The AI Move is: " ++ show aiMove ++ "\n"
        printBoard $ fst newBoard
        gameRound (history ++ [x] ++ [aiMove])


main :: IO ()
main = do
  gameRound []
  readLn

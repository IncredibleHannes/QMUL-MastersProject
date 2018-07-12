{-
  _____|__1__|__2__|__3__|__4__|__5__|__6__|__7__|__8__|__9__|__10__|__11__|__12__|
  time | 0.05| 0.11| 0.22| 0.44| 0.93| 1.87| 3.96| 7.91|16.51| 34.25| 68.33|147.08|
-}

import qualified Data.List as L
import Data.Selection
import Data.Matrix

gameName = "Sudoku"

type R = Bool
type Move = (Int, Int, Int)
type Board = Matrix Int

startingMoves :: [Move]
startingMoves = [{-(1,1,5),-} (1,2,3), (1,3,4), (1,4,6), (1,5,7), (1,6,8), (1,7,9), (1,8,1), (1,9,2),
                 (2,1,6), (2,2,7), {-(2,3,2),-} (2,4,1), (2,5,9), (2,6,5), (2,7,3), (2,8,4), (2,9,8),
                 (3,1,1), {-(3,2,9),-} (3,3,8), (3,4,3), {-(3,5,4),-} (3,6,2), (3,7,5), (3,8,6), (3,9,7),
                 {-(4,1,8),-} (4,2,5), (4,3,9), (4,4,7), (4,5,6), {-(4,6,1),-} (4,7,4), (4,8,2), (4,9,3),
                 (5,1,4), {-(5,2,2),-} (5,3,6), (5,4,8), {-(5,5,5),-} (5,6,3), (5,7,7), (5,8,9), (5,9,1),
                 (6,1,7), (6,2,1), (6,3,3), {-(6,4,9),-} (6,5,2), (6,6,4), (6,7,8), (6,8,5), (6,9,6),
                 (7,1,9), (7,2,6), {-(7,3,1),-} (7,4,5), (7,5,3), (7,6,7), (7,7,2), (7,8,8), (7,9,4),
                 {-(8,1,2),-} (8,2,8), (8,3,7), (8,4,4), (8,5,1), (8,6,9), (8,7,6), (8,8,3), (8,9,5),
                 (9,1,3), (9,2,4), (9,3,5), (9,4,2), (9,5,8), (9,6,6), (9,7,1), (9,8,7){-, (9,9,9)-}]

startingBoard :: Board
startingBoard = outcome startingMoves (zero 9 9)

wins :: Board -> Bool
wins b = row b && row (transpose b) && square b

row :: Board -> Bool
row b = not $ any (containsDuplicates . filter (/= 0)) (toLists b)

square :: Board -> Bool
square b = not $ any (containsDuplicates . filter (/= 0) .  toList) boxes
    where boxes = [submatrix 1 3 1 3 b , submatrix 4 6 4 6 b , submatrix 7 9 7 9 b ,
                  submatrix 4 6 1 3 b , submatrix 4 6 4 6 b , submatrix 4 6 7 9 b ,
                  submatrix 7 9 1 3 b , submatrix 7 9 4 6 b , submatrix 7 9 7 9 b]

containsDuplicates :: [Int] -> Bool
containsDuplicates [] = False
containsDuplicates xs = any (uncurry (==)) . zip s $ tail s
    where s = L.sort xs

outcome :: [Move] -> Board -> Board
outcome [] b     = b
outcome (x:xs) b = let nb = insert x b in if wins nb then outcome xs nb else nb

insert :: Move -> Board -> Board
insert (x, y, z) = setElem z (x, y)

getPossibleMoves :: [Move] -> [Move]
getPossibleMoves xs = filter (\(x, y, z) -> not $ findMove (x, y) xs)
                      [(x,y,z) | x <- [1..9], y <- [1..9], z <- [1..9]]
  where
    findMove :: (Int, Int) -> [Move] -> Bool
    findMove _ []                  = False
    findMove (x, y) ((x1,y1,_):xs) = x == x1 && y == y1 || findMove (x, y) xs

p :: [Move] -> R
p ms = wins(outcome ms startingBoard)

epsilons :: [[Move] -> J R Move]
epsilons = take 5 all
  where all = epsilon' : all
        epsilon' history = epsilonSupBool (getPossibleMoves (startingMoves ++ history))

main :: IO ()
main = do
  let optimalGame = optimalPlay p epsilons
  putStr ("An optimal play for " ++ gameName ++ " is "
     ++ show optimalGame
     ++ "\nand the optimal outcome is " ++ show (p optimalGame) ++ "\n")

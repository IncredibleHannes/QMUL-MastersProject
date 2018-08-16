{-
  Times: 6: 44s |5: 3.77 | 4: 0.54 | 3: 0.41 | 2: 0.11 | 1: 0.10
-}
import qualified Data.List      as L
import           Data.Matrix
import           Data.Selection

gameName = "SimplifiedChess"

type R = (Int, Int)
type Position = (Int, Int)
type Move = (Position, Position)
type Board = Matrix ChessPice
data Colour = W | B
  deriving (Eq, Show)
data ChessPice = N | Queen Colour | King Colour | Rook Colour | Bishop Colour
  deriving (Eq, Show)

startBoard :: Board
startBoard = setElem (Rook W) (1,4) $ setElem (Rook W) (1,1) $ setElem (King W) (2,4) $
             setElem (King B) (8,5) (matrix 8 8 (const N))

wins :: Board -> Colour -> Bool
wins b c =  King (changeColour c) `notElem` toList b

getPosition :: ChessPice -> Board -> [Position]
getPosition p b = getPosition' p b (1,1)
  where
    getPosition' p b (1,9) = []
    getPosition' p b (8,y) = if b ! (8, y) == p then (8,y) : getPosition' p b (1, y+1) else getPosition' p b (1,y+1)
    getPosition' p b (x,y) = if b ! (x, y) == p then (x,y) : getPosition' p b (x+1, y) else getPosition' p b (x+1, y)

value :: Board -> Int
value b  | wins b W  = 1
         | wins b B  = -1
         | otherwise = 0

changeColour :: Colour -> Colour
changeColour B = W
changeColour W = B

getPossibleMoves' :: Board -> Colour -> [Move]
getPossibleMoves' b c = concat [getMoves (x,y) b c | x <- [1..8], y <- [1..8], getMoves (x,y) b c /= [] ]

getPossibleMoves :: [Move] -> [Move]
getPossibleMoves m = concat [getMoves (x,y) b c | x <- [1..8], y <- [1..8], getMoves (x,y) b c /= [] ]
  where
    b = insertMoves startBoard m
    c = if (length m `mod` 2) == 1 then B else W

getMoves :: Position -> Board -> Colour -> [Move]
getMoves p b c1 = case b ! p of
                     N          -> []
                     (King c2)   -> if c1 == c2 then getKingMoves p else []
                     (Queen c2)  -> if c1 == c2 then getQueenMoves p b else []
                     (Rook c2)   -> if c1 == c2 then getRookMoves p b else []
                     (Bishop c2) -> if c1 == c2 then getBishopMoves p b else []

getKingMoves :: Position -> [Move]
getKingMoves (x, y) = map (\m -> ((x,y), m)) (filter (\(x,y) -> not $ x > 8 || y > 8 || x < 1 || y < 1)
                                              [(x, y+1), (x, y-1), (x+1, y), (x-1, y),
                                              (x+1, y+1), (x+1, y-1), (x-1, y+1), (x-1,y-1)])

getQueenMoves :: Position -> Board -> [Move]
getQueenMoves p b = getRookMoves p b ++ getBishopMoves p b

getRookMoves :: Position -> Board -> [Move]
getRookMoves p b = row1 p b 1 ++ row2 p b 1 ++ colum1 p b 1 ++ colum2 p b 1

getBishopMoves :: Position -> Board -> [Move]
getBishopMoves p b = left1 p b 1 ++ left2 p b 1 ++  right1 p b 1 ++ right2 p b 1

row1 :: Position -> Board -> Int -> [Move]
row1 p@(x, y) b i | x + i > 8           = []
                  | b ! (x + i, y) /= N = [(p, (x + i, y))]
                  | otherwise           = (p, (x + i, y)) : row1 p b (i + 1)

row2 :: Position -> Board -> Int -> [Move]
row2 p@(x, y) b i | x - i < 1           = []
                  | b ! (x - i, y) /= N = [(p, (x - i, y))]
                  | otherwise           = (p, (x - i, y)) : row2 p b (i + 1)

colum1 :: Position -> Board -> Int -> [Move]
colum1 p@(x, y) b i | y + i > 8           = []
                    | b ! (x, y + i) /= N = [(p, (x, y + i))]
                    | otherwise           = (p, (x, y + i)) : colum1 p b (i + 1)

colum2 :: Position -> Board -> Int -> [Move]
colum2 p@(x, y) b i | y - i < 1           = []
                    | b ! (x, y - i) /= N = [(p, (x, y - i))]
                    | otherwise           = (p, (x, y - i)) : colum2 p b (i + 1)

left1 :: Position -> Board -> Int -> [Move]
left1 p@(x, y) b i | x + i > 8 || y + i > 8  = []
                   | b ! (x + i, y + i) /= N = [(p, (x + i, y + i))]
                   | otherwise               = (p, (x + i, y + i)) : left1 p b (i + 1)

left2 :: Position -> Board -> Int -> [Move]
left2 p@(x, y) b i | x - i < 1 || y - i < 1  = []
                   | b ! (x - i, y - i) /= N = [(p, (x - i, y - i))]
                   | otherwise               = (p, (x - i, y - i)) : left2 p b (i + 1)

right1 :: Position -> Board -> Int -> [Move]
right1 p@(x, y) b i | x + i > 8 || y - i < 1  = []
                    | b ! (x + i, y - i) /= N = [(p, (x + i, y - i))]
                    | otherwise               = (p, (x + i, y - i)) : right1 p b (i + 1)

right2 :: Position -> Board -> Int -> [Move]
right2 p@(x, y) b i | x - i < 1 || y + i > 8  = []
                    | b ! (x - i, y + i) /= N = [(p, (x - i, y + i))]
                    | otherwise               = (p, (x - i, y + i)) : right2 p b (i + 1)


insertMoves :: Board -> [Move] -> Board
insertMoves = foldl insert

insert :: Board -> Move -> Board
insert b (p1, p2) = if fst p2 > 8 || snd p2 > 8 then undefined else setElem N p1 (setElem (b ! p1) p2 b)

outcome :: Colour -> [Move] -> Board -> Int -> R
outcome _ [] b i       = (value b, i)
outcome c (m : ms) b i = let nb = insert b m in
                         if wins nb c then (value nb, i+1) else outcome (changeColour c) ms nb (i+1)

p :: [Move] -> R
p ms = outcome W ms startBoard 0

epsilons :: [[Move] -> J R Move]
epsilons = take 7 all
  where all = epsilonO : epsilonX : all
        epsilonX history = epsilonMinTupleParalell (getPossibleMoves history)
        epsilonO history = epsilonMaxTupleParalell (getPossibleMoves history)

main :: IO ()
main = do
  let optimalGame = optimalPlay p epsilons
  putStr ("An optimal play for " ++ gameName ++ " is "
     ++ show optimalGame
     ++ "\nand the optimal outcome is " ++ show (p optimalGame) ++ "\n"
     ++ "and the end board is: \n" ++ show (insertMoves startBoard optimalGame))

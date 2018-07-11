module Play (gameName, optimalPlay, optimalOutcome, optimalStrategy) where

import DependentMonadicBigotimes
import J
import Sudoku

optimalPlay :: [Move]
optimalPlay = selection(bigotimes epsilons) p

optimalOutcome :: R
optimalOutcome = p optimalPlay

optimalStrategy :: [Move] -> Move
optimalStrategy as = head(selection(bigotimes epsilons') p')
   where epsilons' = drop (length as) epsilons
         p' xs = p(as ++ xs)

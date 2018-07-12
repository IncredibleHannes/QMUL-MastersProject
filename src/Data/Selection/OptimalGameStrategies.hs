module Data.Selection.OptimalGameStrategies
  ( optimalPlay
  , optimalOutcome
  , optimalStrategy
  ) where

import           Data.Selection.Bigotimes
import           Data.Selection.J

optimalPlay :: ([b] -> a) -> [[b] -> J a b] -> [b]
optimalPlay p epsilons = selection(bigotimes epsilons) p

optimalOutcome :: ([b] -> a) -> [[b] -> J a b] -> a
optimalOutcome p epsilons = p $ optimalPlay p epsilons

optimalStrategy :: ([b] -> a) -> ([b] -> [[b] -> J a b]) -> [b] -> b
optimalStrategy p epsilons as = head(selection(bigotimes epsilons') p')
   where epsilons' = drop (length as) (epsilons as)
         p' xs = p(as ++ xs)

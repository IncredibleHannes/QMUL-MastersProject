  -- | Definition of different minimum and maximum functions

module Data.Selection.MinMax
  ( epsilonMax, epsilonMin
  , epsilonMaxThree, epsilonMinThree
  , epsilonMaxBool, epsilonMinBool
  , epsilonMaxTuple, epsilonMinTuple
  , epsilonMaxParalell, epsilonMinParalell
  , epsilonMaxTupleParalell, epsilonMinTupleParalell
  ) where

import           Control.Parallel.Strategies
import           Data.List
import           Data.Selection.J

-- 3 = {-1,0,1}, represented by the type Int.
type Three = Int

epsilonMin :: (Ord b) => [a] -> J b a
epsilonMin xs = J(epsilonMin' xs)

epsilonMin' :: (Ord b) => [a] -> (a -> b) -> a
epsilonMin' [] _ = undefined
epsilonMin' xs f = head $ sortOn f xs

epsilonMax :: (Ord b) => [a] -> J b a
epsilonMax xs = J(epsilonMax' xs)

epsilonMax' :: (Ord b) => [a] -> (a -> b) -> a
epsilonMax' [] _ = undefined
epsilonMax' xs f = last $ sortOn f xs

epsilonMinParalell ::(NFData a, NFData b, Ord a, Ord b) => [b] -> J a b
epsilonMinParalell xs = J(epsilonMinParalell' xs)

epsilonMinParalell' :: (NFData a, NFData b, Ord a, Ord b) => [a] -> (a -> b) -> a
epsilonMinParalell' xs f = snd $ minimum $ parMap rdeepseq (\x -> (f x, x)) xs

epsilonMaxParalell ::(NFData a, NFData b, Ord a, Ord b) => [b] -> J a b
epsilonMaxParalell xs = J(epsilonMaxParalell' xs)

epsilonMaxParalell' :: (NFData a, NFData b, Ord a, Ord b) => [a] -> (a -> b) -> a
epsilonMaxParalell' xs f = snd $ maximum $ parMap rdeepseq (\x -> (f x, x)) xs

epsilonMinTuple :: [a] -> J (Int, Int) a
epsilonMinTuple xs = J(epsilonMinTuple' xs)

epsilonMinTuple' :: [a] -> (a -> (Int, Int)) -> a
epsilonMinTuple' [] _ = undefined
epsilonMinTuple' xs f = let list = sortOn fst (map (\x -> (f x, x)) xs) in
                            if  fst (fst $ head list) >= 0
                            then snd $ last $ filter (\x -> fst (fst x) == fst (fst $ head list)) list
                            else snd $ head list

epsilonMaxTuple :: [a] -> J (Int, Int) a
epsilonMaxTuple xs = J(epsilonMaxTuple' xs)

epsilonMaxTuple' :: [a] -> (a -> (Int, Int)) -> a
epsilonMaxTuple' [] _ = undefined
epsilonMaxTuple' xs f = let list = reverse $ sortOn fst (map (\x -> (f x, x)) xs) in
                            if  fst (fst $ head list) > 0
                            then snd $ last $ filter (\x -> fst (fst x) == fst (fst $ head list)) list
                            else snd $ head list

epsilonMinTupleParalell :: (NFData a) => [a] -> J (Int, Int) a
epsilonMinTupleParalell xs = J(epsilonMinTupleParalell' xs)


epsilonMinTupleParalell' :: (NFData a) => [a] -> (a -> (Int, Int)) -> a
epsilonMinTupleParalell' [] _ = undefined
epsilonMinTupleParalell' xs f = let list = sortOn fst (parMap rdeepseq (\x -> (f x, x)) xs) in
                            if  fst (fst $ head list) >= 0
                            then snd $ last $ filter (\x -> fst (fst x) == fst (fst $ head list)) list
                            else snd $ head list

epsilonMaxTupleParalell :: (NFData a) => [a] -> J (Int, Int) a
epsilonMaxTupleParalell xs = J(epsilonMaxTupleParalell' xs)

epsilonMaxTupleParalell' :: (NFData a) => [a] -> (a -> (Int, Int)) -> a
epsilonMaxTupleParalell' [] _ = undefined
epsilonMaxTupleParalell' xs f = let list = reverse $ sortOn fst (parMap rdeepseq (\x -> (f x, x)) xs) in
                            if  fst (fst $ head list) > 0
                            then snd $ last $ filter (\x -> fst (fst x) == fst (fst $ head list)) list
                            else snd $ head list

epsilonMinThree :: [a] -> J Three a
epsilonMinThree xs = J(epsilonMin' xs)

epsilonMinThree' :: [a] -> (a -> Three) -> a
epsilonMinThree' [] _     = undefined
epsilonMinThree' [x] _    = x
epsilonMinThree' (x:xs) f | f x == 1 = epsilonMinThree' xs f
                          | f x == -1  = x
                          | otherwise = case findMin xs f of
                                          (Just y) -> y
                                          Nothing  -> x
    where findMin [] _     = Nothing
          findMin (x:xs) f = if f x == -1 then Just x else findMin xs f

epsilonMaxThree :: [a] -> J Three a
epsilonMaxThree xs = J(epsilonMax' xs)

epsilonMaxThree' :: [a] -> (a -> Three) -> a
epsilonMaxThree' [] _     = undefined
epsilonMaxThree' [x] _    = x
epsilonMaxThree' (x:xs) f | f x == -1 = epsilonMaxThree' xs f
                          | f x == 1  = x
                          | otherwise = case findMax xs f of
                                          (Just y) -> y
                                          Nothing  -> x
    where findMax [] _     = Nothing
          findMax (x:xs) f = if f x == 1 then Just x else findMax xs f

epsilonMinBool :: [a] -> J Bool a
epsilonMinBool xs = J(epsilonMinBool' xs)

epsilonMinBool' :: [a] -> (a -> Bool) -> a
epsilonMinBool' [] _     = undefined
epsilonMinBool' [x] _    = x
epsilonMinBool' (x:xs) f = if not $ f x then x else epsilonMinBool' xs f

epsilonMaxBool :: [a] -> J Bool a
epsilonMaxBool xs = J(epsilonMaxBool' xs)

epsilonMaxBool' :: [a] -> (a -> Bool) -> a
epsilonMaxBool' [] _     = undefined
epsilonMaxBool' [x] _    = x
epsilonMaxBool' (x:xs) f = if f x then x else epsilonMaxBool' xs f

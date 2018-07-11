module InfSupSelections (Three,
                         epsilonInf, epsilonSup,
                         epsilonInfThree, epsilonSupThree,
                         epsilonInfBool, epsilonSupBool,
                         epsilonParalellMax)
where

import J
import Control.Parallel.Strategies
import Data.List

-- 3 = {-1,0,1}, represented by the type Int.
type Three = Int

epsilonInf :: (Ord b) => [a] -> J b a
epsilonInf xs = J(epsilonInf' xs)

epsilonInf' :: (Ord b) => [a] -> (a -> b) -> a
epsilonInf' [] _ = undefined
epsilonInf' xs f = head $ sortOn f xs

epsilonSup :: (Ord b) => [a] -> J b a
epsilonSup xs = J(epsilonSup' xs)

epsilonSup' :: (Ord b) => [a] -> (a -> b) -> a
epsilonSup' [] _ = undefined
epsilonSup' xs f = head $ reverse $ sortOn f xs

epsilonInfThree :: [a] -> J Three a
epsilonInfThree xs = J(epsilonInf' xs)

epsilonInfThree' :: [a] -> (a -> Three) -> a
epsilonInfThree' [] _     = undefined
epsilonInfThree' [x] _    = x
epsilonInfThree' (x:xs) f | f x == 1 = epsilonInfThree' xs f
                          | f x == -1  = x
                          | otherwise = case findInf xs f of
                                          (Just y)  -> y
                                          (Nothing) -> x
    where findInf [] _     = Nothing
          findInf (x:xs) f = if f x == -1 then Just x else findInf xs f

epsilonSupThree :: [a] -> J Three a
epsilonSupThree xs = J(epsilonSup' xs)

epsilonSupThree' :: [a] -> (a -> Three) -> a
epsilonSupThree' [] _     = undefined
epsilonSupThree' [x] _    = x
epsilonSupThree' (x:xs) f | f x == -1 = epsilonSupThree' xs f
                          | f x == 1  = x
                          | otherwise = case findSup xs f of
                                          (Just y)  -> y
                                          (Nothing) -> x
    where findSup [] _     = Nothing
          findSup (x:xs) f = if f x == 1 then Just x else findSup xs f

epsilonInfBool :: [a] -> J Bool a
epsilonInfBool xs = J(epsilonInfBool' xs)

epsilonInfBool' :: [a] -> (a -> Bool) -> a
epsilonInfBool' [] _     = undefined
epsilonInfBool' [x] _    = x
epsilonInfBool' (x:xs) f = if not $ f x then x else epsilonInfBool' xs f

epsilonSupBool :: [a] -> J Bool a
epsilonSupBool xs = J(epsilonSupBool' xs)

epsilonSupBool' :: [a] -> (a -> Bool) -> a
epsilonSupBool' [] _     = undefined
epsilonSupBool' [x] _    = x
epsilonSupBool' (x:xs) f = if f x then x else epsilonSupBool' xs f

epsilonParalellMax ::(NFData a, NFData b, Ord a, Ord b) => [b] -> J a b
epsilonParalellMax xs = J(epsilonParalellMax' xs)

epsilonParalellMax' :: (NFData a, NFData b, Ord a, Ord b) => [a] -> (a -> b) -> a
epsilonParalellMax' xs f = snd $ head $ sort $ parMap rdeepseq (\x -> (f x, x)) xs

epsilonParalellMin ::(NFData a, NFData b, Ord a, Ord b) => [b] -> J a b
epsilonParalellMin xs = J(epsilonParalellMin' xs)

epsilonParalellMin' :: (NFData a, NFData b, Ord a, Ord b) => [a] -> (a -> b) -> a
epsilonParalellMin' xs f = snd $ head $ reverse $ sort $ parMap rdeepseq (\x -> (f x, x)) xs

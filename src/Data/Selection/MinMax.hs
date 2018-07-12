
module Data.Selection.MinMax (Three,
                         epsilonSup, epsilonInf,
                         epsilonSupThree, epsilonInfThree,
                         epsilonSupBool, epsilonInfBool,
                         epsilonSupTuple, epsilonInfTuple,
                         epsilonSupParalell, epsilonInfParalell,
                         epsilonSupTupleParalell, epsilonInfTupleParalell)
where

import           Control.Parallel.Strategies
import           Data.List
import           Data.Selection.J

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
epsilonSup' xs f = last $ sortOn f xs

epsilonInfTuple :: [a] -> J (Int, Int) a
epsilonInfTuple xs = J(epsilonInfTuple' xs)


epsilonInfTuple' :: [a] -> (a -> (Int, Int)) -> a
epsilonInfTuple' [] _ = undefined
epsilonInfTuple' xs f = let list = sortOn fst (map (\x -> (f x, x)) xs) in
                            if  fst (fst $ head list) > 0
                            then snd $ last $ filter (\x -> fst (fst x) == fst (fst $ head list)) list
                            else snd $ head list

epsilonSupTuple :: [a] -> J (Int, Int) a
epsilonSupTuple xs = J(epsilonSupTuple' xs)

-- TODO: Find more performant version
epsilonSupTuple' :: [a] -> (a -> (Int, Int)) -> a
epsilonSupTuple' [] _ = undefined
epsilonSupTuple' xs f = let list = reverse $ sortOn fst (map (\x -> (f x, x)) xs) in
                            if  fst (fst $ head list) > 0
                            then snd $ last $ filter (\x -> fst (fst x) == fst (fst $ head list)) list
                            else snd $ head list

epsilonInfTupleParalell :: (NFData a) => [a] -> J (Int, Int) a
epsilonInfTupleParalell xs = J(epsilonInfTupleParalell' xs)


epsilonInfTupleParalell' :: (NFData a) => [a] -> (a -> (Int, Int)) -> a
epsilonInfTupleParalell' [] _ = undefined
epsilonInfTupleParalell' xs f = let list = sortOn fst (map (\x -> (f x, x)) xs) in
                            if  fst (fst $ head list) > 0
                            then snd $ last $ filter (\x -> fst (fst x) == fst (fst $ head list)) list
                            else snd $ head list

epsilonSupTupleParalell :: (NFData a) => [a] -> J (Int, Int) a
epsilonSupTupleParalell xs = J(epsilonSupTuple' xs)

epsilonSupTupleParalell' :: (NFData a) => [a] -> (a -> (Int, Int)) -> a
epsilonSupTupleParalell' [] _ = undefined
epsilonSupTupleParalell' xs f = let list = reverse $ sortOn fst (parMap rdeepseq (\x -> (f x, x)) xs) in
                            if  fst (fst $ head list) > 0
                            then snd $ last $ filter (\x -> fst (fst x) == fst (fst $ head list)) list
                            else snd $ head list


epsilonInfThree :: [a] -> J Three a
epsilonInfThree xs = J(epsilonInf' xs)

epsilonInfThree' :: [a] -> (a -> Three) -> a
epsilonInfThree' [] _     = undefined
epsilonInfThree' [x] _    = x
epsilonInfThree' (x:xs) f | f x == 1 = epsilonInfThree' xs f
                          | f x == -1  = x
                          | otherwise = case findInf xs f of
                                          (Just y) -> y
                                          Nothing  -> x
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
                                          (Just y) -> y
                                          Nothing  -> x
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

epsilonInfParalell ::(NFData a, NFData b, Ord a, Ord b) => [b] -> J a b
epsilonInfParalell xs = J(epsilonInfParalell' xs)

epsilonInfParalell' :: (NFData a, NFData b, Ord a, Ord b) => [a] -> (a -> b) -> a
epsilonInfParalell' xs f = snd $ minimum $ parMap rdeepseq (\x -> (f x, x)) xs

epsilonSupParalell ::(NFData a, NFData b, Ord a, Ord b) => [b] -> J a b
epsilonSupParalell xs = J(epsilonSupParalell' xs)

epsilonSupParalell' :: (NFData a, NFData b, Ord a, Ord b) => [a] -> (a -> b) -> a
epsilonSupParalell' xs f = snd $ maximum $ parMap rdeepseq (\x -> (f x, x)) xs

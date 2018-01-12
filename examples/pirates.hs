{-
Oxford entry exam question:

A group of seven pirates has 100 gold coins. They have to
decide amongst themselves how to divide the treasure, but
must abide by pirate rules:
 - The most senior pirate proposes the division.
 - All of the pirates (including the most senior) vote on
   the division. If half or more vote for the division, it stands.
  If less than half vote for it, they throw the most senior pirate
  overboard and start again.
What division should the most senior pirate suggest to the other six?

-}

-- SELECTION MONAD

import Data.List
import Data.Ord
import Debug.Trace
import Control.Monad (liftM, ap)
import Control.Parallel.Strategies

data J r x = J {selection :: (x -> r) -> x}

monJ :: J r x -> (x -> J r y) -> J r y
monJ e f = J (\p -> b p (a p))
   where
      a p = selection e $ (\x -> p (b p x))
      b p x = selection (f x) p 

instance Functor (J r) where
  fmap = liftM

{- -- We need this for the ghc library > 7.6.3
instance Applicative (J r) where
  pure = return
  (<*>) = ap
-}

instance Monad (J r) where
  return x = J(\p -> x)
  (>>=) = monJ

pr :: (J r x, J r y) -> ((x,y) -> r) -> (x,y)
pr (e, d) q = (a, b a)
  where b x = selection d (\y -> q (x,y))
        a = selection e (\x -> q(x, b x))

prod :: (J r x, J r y) -> J r (x,y)
prod (e,d) = J (pr (e,d))

-- GAME SETUP

-- Two possible moves Share or Vote

-- Share contains the distribution of coins for each pirate
type Pirate = Int

-- Share contains the distribution of coins for each pirate
type Share = [Int]

-- Whether a pirate agrees with share
type Vote = Bool

-- Collection of all votes
type Poll = [Vote]

{-
-- Generic argmax
argmax :: (Ord b) => [a] -> (a -> b) -> [a]
argmax d p = [ x | x <- d, p x == maximum [ p x | x <- d ] ]
-}

{-

If using parallel version of argmax compile as

$ ghc -O2 -threaded --make pirates.hs

And run it as

$ time ./pirates +RTS -N

-}

-- Generic argmax with parallel map
argmax :: (NFData a, NFData b, Ord a, Ord b) => [a] -> (a -> b) -> [a]
argmax d p = [ x | (v,x) <- graph, v == (fst.last $ graph) ]
  where graph = sort $ parMap rdeepseq (\x -> (p x, x)) d

-- All possible ways to divide n coins amongst i pirates
divide :: Int -> Int -> [Share]
divide n 1 = [[n]]
divide n i = [ k:xs | k <- [0..n], xs <- divide (n-k) (i-1) ]

-- Number of coins
nc = 5

-- Number of pirates
np = 5

-- Current most senior, local strategy
s :: Int -> (Share -> Share) -> Share
s i p = snd.last.sort $ [ (xs !! i, xs) | xs <- opt_moves ]
  where shares = divide nc (np - i)
        zs = replicate i 0
        opt_moves = argmax (map (zs++) shares) ((!!i).p)

ss :: Int -> J Share Share
ss i = J (s i)

-- Voting pirates strategy. True means agrees with sharing
v :: Pirate -> (Vote -> Share) -> Vote
v i p = ((!!i).p $ True) > ((!!i).p $ False)

sv :: Pirate -> J Share Vote
sv i = J (v i)

sp :: Pirate -> J Share Poll
sp i = sequence (map sv [(i+1)..(np-1)])

e :: Int -> J Share (Share, Poll)
e i = prod (ss i, sp i)

g :: J Share [(Share, Poll)]
g = sequence (map e [0..(np-1)])

q :: [(Share, Poll)] -> Share
q [(s,v)] = s
q ((s,v):xs) = if 1 + pro >= con then s else q xs
   where [con,pro] = (map length).group.sort $ True:False:v

main = do
  print.head $ selection g q

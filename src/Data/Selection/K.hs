
-- | Definition of the quantifier monad
--
-- Definition is taken from: https://www.cs.bham.ac.uk/~mhe/papers/msfp2010/MSFP2010/haskell/modular/monadic/


module Data.Selection.K (K(K), quantifier, unitK, functorK, muK) where

import           Control.Monad (ap, liftM)

-- | Definition of the quantifier type. The type variable R can be seen as a generalised
-- truth value. For example for r = Bool, a function can be build with the type:
--
-- > f :: [x] -> K Bool x
-- is returning true if an element exists for wich the function:
--
-- > f2 :: (x -> r)
-- is returning true.
newtype K r x = K {quantifier :: (x -> r) -> r}

unitK :: x -> K r x
unitK x = K(\p -> p x)

functorK :: (x -> y) -> K r x -> K r y
functorK f phi = K(\q -> quantifier phi(\x -> q(f x)))

muK :: K r (K r x) -> K r x
muK phi = K(\p -> quantifier phi (\gamma -> quantifier gamma p))

instance Monad (K r) where
  return = unitK
  phi >>= f = muK(functorK f phi)


instance Functor (K r) where
  fmap = liftM

instance Applicative (K r) where
  pure = return
  (<*>) = ap

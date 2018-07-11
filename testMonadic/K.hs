module K (K(K), quantifier, unitK, functorK, muK) where

import Control.Monad (liftM, ap)

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

 -- We need this for the ghc library > 7.6.3
instance Applicative (K r) where
  pure = return
  (<*>) = ap

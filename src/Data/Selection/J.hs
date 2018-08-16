-- | Definition of the selection monad
--
-- Definition is taken from: https://www.cs.bham.ac.uk/~mhe/papers/msfp2010/MSFP2010/haskell/modular/monadic/

module Data.Selection.J
  (J(J), selection, unitJ, functorJ, muJ, morphismJK)
  where

import           Control.Monad    (ap, liftM)
import           Data.Selection.K

-- | Definition of the selection type. The type variable R can be seen as a generalised
-- truth value. For example for r = Bool, a function can be build with the type:
--
-- > f :: [x] -> K Bool x
-- that is selecting an element out of the list of x's for wich the given functions
--
-- > f2 :: (x -> r)
-- is returning true.
newtype J r x = J {selection :: (x -> r) -> x}

-- | Transforing a selection function into a quantifier function
morphismJK :: J r x -> K r x
morphismJK e = K(\p -> p(selection e p))

-- | implementing the return function of the monad definition
unitJ :: x -> J r x
unitJ x = J(\p -> x)

functorJ :: (x -> y) -> J r x -> J r y
functorJ f e = J(\q -> f(selection e (\x -> q(f x))))

muJ :: J r(J r x) -> J r x
muJ e = J(\p -> selection(selection e(\d -> quantifier(morphismJK d) p)) p)

instance Monad (J r) where
  return = unitJ
  e >>= f = muJ(functorJ f e)

instance Functor (J r) where
  fmap = liftM

instance Applicative (J r) where
  pure = return
  (<*>) = ap

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Selection
-- Copyright   :  (c) Johannes Hartmann
-- License     :  MIT
--
-- Maintainer  :  Johannes.Hartmann.Calw@web.de
-- Stability   :  stable
-- Portability :  portable
--
-- The recently discovert selection monad provides an elegant way of implementing
-- AI's for sequential games.
-- This library is providing the definition of the selection and quantifier monad
-- as introduced by Martin Escardó and Paulo Oliva in their paper from 2010:
-- https://www.cs.bham.ac.uk/~mhe/papers/msfp2010/Escardo-Oliva-MSFP2010.pdf
--
-- A detailed introduction to this library as well as some example game
-- implementations can be found in the master thesis of Johannes Hartmann:
-- https://github.com/IncredibleHannes/QMUL-MastersProject
--
-- The library consists of the type definitions of the selection and quantifier
-- monad, the definition of the otimes functions, a collection of minimum
-- and maximum functions and a set of functions that compute the optimal play for
-- a given sequential game.
--
-----------------------------------------------------------------------------


module Data.Selection
  (
  -- * Selection Monad
  -- | Definition of the selection monad
  --
  -- Definition is taken from: https://www.cs.bham.ac.uk/~mhe/papers/msfp2010/MSFP2010/haskell/modular/monadic/
    J(J)
  , selection
  , morphismJK

  -- * Quantifier Monad
  -- | Definition of the quantifier monad
  --
  -- Definition is taken from: https://www.cs.bham.ac.uk/~mhe/papers/msfp2010/MSFP2010/haskell/modular/monadic/
  , K(K)
  , quantifier

  -- * Bigotimes
  -- | Definition of the selection monad
  --
  -- Definition is taken from: https://www.cs.bham.ac.uk/~mhe/papers/msfp2010/MSFP2010/haskell/modular/monadic/
  , varotimes
  , otimes
  , bigotimes

  -- * Min|max functions
  -- | Definition of different minimum and maximum functions

  -- ** Generic
  , epsilonMin
  , epsilonMax
  -- ** Win|Draw|Lose
  , epsilonMinThree
  , epsilonMaxThree
  -- ** Booleans
  , epsilonMinBool
  , epsilonMaxBool
  -- ** Length tracking tuples
  , epsilonMinTuple
  , epsilonMaxTuple
  -- ** Paralell versions
  -- *** Generic
  , epsilonMaxParalell
  , epsilonMinParalell
  -- *** Length tracking tuples
  , epsilonMaxTupleParalell
  , epsilonMinTupleParalell

  -- * Optimal game strategies
  -- | Definition of the functions that calculate the optimal play
  , optimalPlay
  , optimalOutcome
  , optimalStrategy
  )
  where

import           Data.Selection.Bigotimes
import           Data.Selection.J
import           Data.Selection.K
import           Data.Selection.MinMax
import           Data.Selection.OptimalGameStrategies

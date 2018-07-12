-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List
-- Copyright   :  (c) Johannes Hartmann
-- License     :  MIT
--
-- Maintainer  :  Johannes.Hartmann.Calw@web.de
-- Stability   :  stable
-- Portability :  portable
--
-- Operations the Selection Monad
--
-----------------------------------------------------------------------------


module Data.Selection
  (
  -- * Selection Monad J
    J(J)
  , selection
  , morphismJK

  -- * Quantifier Monad K
  , K(K)
  , quantifier

  -- * Bigotimes
  , varotimes
  , otimes
  , bigotimes

  -- * minimum and maximum implementations
  -- ** generic implementations
  , epsilonInf
  , epsilonSup
  -- ** for {-1,0,1}
  , epsilonInfThree
  , epsilonSupThree
  -- ** for booleans
  , epsilonInfBool
  , epsilonSupBool
  -- ** for length tracking tuples ({1,0,-1}, Int)
  , epsilonInfTuple
  , epsilonSupTuple
  -- ** paralell implementations
  -- *** generic
  , epsilonParalellMax
  , epsilonParalellMin
  -- *** length tracking tuples ({1,0,-1}, Int)
  , epsilonSupTupleParalell
  , epsilonInfTupleParalell

  -- * Optimal game strategies
  , optimalPlay
  , optimalOutcome
  , optimalStrategy
  )
  where

import Data.Selection.J
import Data.Selection.K
import Data.Selection.Bigotimes
import Data.Selection.MinMax
import Data.Selection.OptimalGameStrategies

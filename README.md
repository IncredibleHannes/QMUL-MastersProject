# Selection Monad

## Library

## Example Game
With the library some example games where implemented. Some of this examples an
external matrix library which can be installed with the following command:

  > cabal install matrix

Make sure to execute the games you have a current version of GHCI and cabal.
Each of the example case studies have their own main method and therefore need
to be compiled and executed individually.  
Furthermore, some implementations support parallelism and need therefore be
compiled with the following command:

  >  ghc -O2 -threaded --make <FileName.hs>

## Tests
This application provides a set of tests. To run the tests execute `stack test`
in the main folder.

## Documentation
The haddoc documentation of the library can be found at:
https://incrediblehannes.github.io/QMUL-MastersProject/

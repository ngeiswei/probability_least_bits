#!/usr/bin/runhaskell

import Math.Combinatorics.Binomial
import System.Environment
import Data.Number.CReal
import Text.Format

-- Little script to compute the probability of having at least t 1s in
-- a n-bits string, given that the probability of having 1 in any of
-- the bit is p.

main :: IO ()
main = do
  [n_str, t_str, p_str] <- getArgs
  let n = read n_str :: Int
      t = read t_str :: Int
      p = read p_str :: CReal
  putStrLn (format "Probability of having at least {0} 1-bits amonst {1} = {2}"
            [t_str, n_str, (show (patleast n t p))])
  putStrLn ("Probability of the universe " ++ 
            "(should be 1.0, otherwise there is a problem) = " ++
            (show (puniverse n t p)))

-- Probability of having exactly k 1-bits:
--
-- pexact(n, k, p) = C(n, k) * p^k * p^(n-k)
pexact :: Int -> Int -> CReal -> CReal
pexact n k p = (p**n1 * (1-p)**n0) * (fromIntegral (choose n k))
  where n1 = fromIntegral k
        n0 = fromIntegral (n-k)

-- Probability of having at least t 1-bits:
--
-- pcomb(n, t, p) = Sum_{k=t}^n pexact(n, k, p)
patleast :: Int -> Int -> CReal -> CReal
patleast n t p = sum [pexact n k p | k <- [t..n]]

-- Probability of the universe (to check the inaccuracies of the
-- computation)
puniverse :: Int -> Int -> CReal -> CReal
puniverse n t p = sum [pexact n k p | k <- [0..n]]

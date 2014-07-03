#!/usr/bin/runhaskell

import Math.Combinatorics.Binomial
import System.Environment
import Data.Number.CReal
import Text.Format

-- Little script to compute the probability of having at least t 1s in
-- a n-bit string, given that the probability of having 1 in any of
-- the bit is p.

main :: IO ()
main = do
  [n_str, t_str, p_str] <- getArgs
  let n = read n_str :: Int
      t = read t_str :: Int
      p = read p_str :: CReal
  putStrLn (format "Probability of having at least {0} 1s in a {1}-bit string = {2}"
            [t_str, n_str, (show (p_atleast n t p))])
  putStrLn ("Probability of the universe " ++ 
            "(should be 1.0, otherwise there is a problem) = " ++
            (show (p_universe n t p)))

-- Probability of having exactly k 1s in a n-bit string:
--
-- p_exact(n, k, p) = C(n, k) * p^k * p^(n-k)
p_exact :: Int -> Int -> CReal -> CReal
p_exact n k p = (p**n1 * (1-p)**n0) * (fromIntegral (choose n k))
  where n1 = fromIntegral k
        n0 = fromIntegral (n-k)

-- Probability of having at least t 1s in a n-bit string:
--
-- p_atleast(n, t, p) = Sum_{k=t}^n p_exact(n, k, p)
p_atleast :: Int -> Int -> CReal -> CReal
p_atleast n t p = sum [p_exact n k p | k <- [t..n]]

-- Probability of the universe (to check the inaccuracies of the
-- computation)
p_universe :: Int -> Int -> CReal -> CReal
p_universe n t p = sum [p_exact n k p | k <- [0..n]]

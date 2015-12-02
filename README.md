# Overview

Little script to compute the probability of having at least t 1s in a
n-bit string, given that the probability of having 1 in any bit is p.

# Requirements

You need the following packages:

* numbers
* exact-combinatorics
* text-format-simple

For Haskell noobs, here's a possible way to install them

```
$ cabal install numbers exact-combinatorics text-format-simple
```

# Usage

Just run it on the command line

```
$ ./probability_least_bits.hs N T P
```

to calculate the probability of having at least T 1-bits in a string
of N bits given that probability of 1-bit is P.


module Arithmetic.Tests.Int where

import Test.Scher

addition_commutative = 
  forAll "i" $ \i ->
    forAll "j" $ \j ->
      i + j == j + (i :: Integer)

addOne_monotonic =
  forAll "i" $ \i ->
    i < (i + 1 :: Int)

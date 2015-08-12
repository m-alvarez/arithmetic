module Arithmetic.Tests.Int.Incorrect where

-- This fails because either number might be negative
addition_monotonic :: Int -> Int -> Bool
addition_monotonic i j = i + j > i

-- This fails when i = MAXINT
addition_overflow :: Int -> Bool
addition_overflow i = i + 1 > i

-- This fails because i might be negative
multiplication_monotonic :: Int -> Bool
multiplication_monotonic i = i * 2 > i

-- This fails because 2i might overflow
multiplication_overflow :: Int -> Bool
multiplication_overflow i = if i > 0 then i * 2 > i else i * 2 <= i

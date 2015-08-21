module Arithmetic.Tests.Int.Incorrect where

-- This fails because either number might be negative
addition_monotonic :: Int -> Int -> Bool
addition_monotonic i j = i + j > i

-- This fails because overflow
addition_overflow :: Int -> Bool
addition_overflow i = i + 1 > i

-- This fails because i might be negative
multiplication_monotonic :: Int -> Bool
multiplication_monotonic i = 2 * i >= i

-- This fails because 2i might overflow NEVERMIND IT ACTUALLY DOESN'T FAIL
multiplication_overflow :: Int -> Bool
multiplication_overflow i = i <= 0 || i < 2 * i

multiplication_overflow_rem :: Int -> Bool
multiplication_overflow_rem i = i `rem` 9 < 8 || i < 2 * i


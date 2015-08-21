module Arithmetic.Tests.Int.Incorrect where

addition_monotonic :: Int -> Int -> Bool
addition_monotonic i j = i + j > i

addition_overflow :: Int -> Bool
addition_overflow i = i + 1 > i

multiplication_monotonic :: Int -> Bool
multiplication_monotonic i = 2 * i >= i

multiplication_overflow :: Int -> Bool
multiplication_overflow i = i <= 0 || i < 2 * i

multiplication_overflow_rem :: Int -> Bool
multiplication_overflow_rem i = i `rem` 9 < 8 || i < 2 * i


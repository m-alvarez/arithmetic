module Arithmetic.Tests.Int.Correct where

addition_commutative :: Int -> Int -> Bool
addition_commutative i j = i + j == j + i

addition_associative :: Int -> Int -> Int -> Bool
addition_associative i j k = i + (j + k) == (i + j) + k

addition_inverse :: Int -> Bool
addition_inverse i = i + (-i) == 0

addition_neutral :: Int -> Bool
addition_neutral i = i + 0 == i

multiplication_commutative :: Int -> Int -> Bool
multiplication_commutative i j = i * j == j * i

multiplication_associative :: Int -> Int -> Int -> Bool
multiplication_associative i j k = i * (j * k) == (i * j) * k

multiplication_neutral :: Int -> Bool
multiplication_neutral i = i * 0 == 0

multiplication_distributive :: Int -> Int -> Int -> Bool
multiplication_distributive i j k = (i + j) * k == i * k + j * k

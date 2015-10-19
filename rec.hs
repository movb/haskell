module Rec where

factorial n = if n == 0 then 1 else n*factorial(n-1)

factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

factorial'' 0 = 1
factorial'' n = if n<0 then error "arg must be > 0" else n * factorial' (n - 1)

factorial''' 0 = 1
factorial''' n | n < 0 = error "arg must be > 0" 
               | n > 0 = n * factorial' (n - 1)

factorial4 :: Integer -> Integer
factorial4 n | n == 0     = 1
             | n > 0      = n * factorial' (n - 1)
             | otherwise  = error "arg must be > 0" 

fibonacci :: Integer -> Integer
fibonacci n | n == 0    = 0
            | n == 1    = 1
            | n > 1     = fibonacci(n-1) + fibonacci(n-2)
            | n < 0     = fibonacci(n+2) - fibonacci(n+1)

factorial5 n | n>=0      = helper 1 n
             | otherwise = error "error"

helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)

fibonacciL :: Integer -> Integer
fibonacciL n | n == 0   = 0
             | n == 1   = 1
             | n > 1    = fhelper1 0 1 n
             | n < 0    = fhelper2 0 1 n

fhelper1 acc1 acc2 1 = acc2
fhelper1 acc1 acc2 n = fhelper1 acc2 (acc1 + acc2) (n-1)

fhelper2 acc1 acc2 0 = acc1
fhelper2 acc1 acc2 n = fhelper2 (acc2 - acc1) (acc1) (n+1)

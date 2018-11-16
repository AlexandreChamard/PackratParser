module Main where


fib 0 = 0 ; fib 1 = 1 ; fib n = fib (n - 2) + fib (n - 1)

fibs = 0 : 1 : (zipWith (+) fibs (tail fibs))
fib' n = fibs !! n

fac 0 = 1 ; fac n = n * fac (n - 1)

-- facs = 1 : (zipWith (*) fac)
-- fac' n = facs !! n

-- primes = [x | x <- [2..], y <- [2..x], z <- [x..y], x ]

arr = [(x,y,z) | z <- [1..], x <- [1..z], y <- [x..z], z^2 == x^2 + y^2 ]

main = do
    -- print $ fib 35
    -- print $ [fib 35, fib 35]
    print $ fib' 35
    print $ [fib' 35, fib' 35]

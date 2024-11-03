-- 아름답다...
primes = [x | x <- [2..1000], not $ any (\n -> mod x n == 0) [2..x-1]]

pythTriples = [
    (a, b, c) |
        c <- [1..100],
        b <- [1..c],
        a <- [1..b],
        a^2 + b^2 == c^2
    ]

main :: IO ()
main = do 
    print primes
    print pythTriples
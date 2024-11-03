--프라임 숫자 판별기

isPrime :: Int -> Bool
isPrime n
    | n < 2 = False
    | otherwise = not (hasDivisor(n-1))
    where
        hasDivisor :: Int -> Bool
        hasDivisor 1 = False
        hasDivisor x = mod n x == 0 || hasDivisor (x-1)


main :: IO ()
main = do
    print $ isPrime 0
    print $ isPrime 1
    print $ isPrime 2
    print $ isPrime 3
    print $ isPrime 4
    print $ isPrime 5
    print $ isPrime 6
    print $ isPrime 7
    print $ isPrime 8
    print $ isPrime 9
    print $ isPrime 10
    print $ isPrime 11
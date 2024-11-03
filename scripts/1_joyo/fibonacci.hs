fib :: Int -> Int

fib 0 = 0 -- base case
fib 1 = 1 -- base case
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
    print $ fib 0
    print $ fib 1
    print $ fib 2
    print $ fib 3
    print $ fib 4
    print $ fib 5
    print $ fib 6
    print $ fib 7
    print $ fib 8
    print $ fib 9
    print $ fib 10
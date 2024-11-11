add :: Int -> Int -> Int
add x y = x + y

mul2 = (* 2)

main :: IO ()
main = do
    -- Using the defined add function
    print $ add 1 2
    -- Using the built-in (+) operator directly
    print $ 1 + 2
    -- Using point-free style instead of lambda
    print $ (+) 1 2
    -- Using lambda with multiple parameters instead of add
    print $ (\x y -> x + y) 1 2
    -- Using lambda function differently
    print $ (\x -> \y -> x + y) 1 2

    print $ mul2 5
    print $ (\x -> x * 2) 5
    

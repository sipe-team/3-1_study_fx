add :: Int -> Int -> Int
add x y = x + y

main :: IO ()
main = do
    -- Using the defined add function
    print $ add 1 2
    -- Using the built-in (+) operator directly
    print $ 1 + 2
    -- Using point-free style instead of lambda
    print $ (+) 1 2
    -- Using lambda instead of add
    print $ (\x y -> x + y) 1 2

power :: Int -> Int -> Int

power x 0 = 1  -- Base Case
power x p
    | even p = n * n
    | otherwise = n * n * x
    where
        n = power x (div p 2)
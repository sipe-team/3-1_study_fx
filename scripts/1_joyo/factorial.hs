factorial :: Integer -> Integer
factorial 0 = 1
factorial n 
    | n < 0     = error "Factorial is not defined for negative numbers"
    | otherwise = n * factorial (n - 1)

double n = n * 2
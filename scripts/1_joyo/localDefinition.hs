import Control.Concurrent (yield)
-- 원의 면적과 둘레 계산
circleCalculation :: Double -> (Double, Double)
circleCalculation radius =
    let area = pi * (radius ** 2)
        circumference = 2 * pi * radius
        in (area, circumference)

circleCalculation2 :: Double -> (Double, Double)
circleCalculation2 radius = (area, circumference)
    where
        area = pi * (radius ** 2)
        circumference = 2 * pi * radius


-- 피타고라스 정리
pythagorean :: Double -> Double -> Double
pythagorean a b =
    let squareA = a ** 2
        squareB = b ** 2
    in sqrt $ squareA + squareB

-- 지수 계산
fastExp :: Integer -> Integer -> Integer
fastExp _ 0 = 1
fastExp x n
    | even n    = y * y
    | otherwise = y * y * x
    where
        n_halved = div n 2
        y = fastExp x n_halved

fastExp2 :: Integer -> Integer -> Integer
fastExp2 _ 0 = 1
fastExp2 x n =
    let n_halved = div n 2
        y = fastExp2 x n_halved
    in
        if even n
        then y * y
        else y * y * x

fibonacci :: Integer -> Integer
fibonacci n =
    let fib 0 = 1
        fib 1 = 1
        fib n' = fib (n'-1) + fib (n'-2)
    in  fib n

fibonacci2 :: Integer -> Integer
fibonacci2 = fib
    where
        fib 0 = 1
        fib 1 = 1
        fib n' = fib (n'-1) + fib (n'-2)


main :: IO ()
main = do
    print $ circleCalculation 2
    print $ pythagorean 3 4
    print $ fastExp 3 4
    print $ fibonacci 5
    print $ fibonacci2 5
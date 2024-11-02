-- 원의 면적과 둘레 계산

circleCalculation :: Double -> (Double, Double)
circleCalculation radius =
    let area = pi * radius ** 2
        circumference = 2 * pi * radius
        in (area, circumference)

main :: IO ()
main = do
    print $ circleCalculation 10
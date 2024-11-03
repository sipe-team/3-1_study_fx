-- 도형 면적 계산

-- case 구문
calculateAreaCase :: String -> Double -> Double -> Double
calculateAreaCase shape width height =
    case shape of
        "rectangle" -> width * height
        "triangle"  -> width * height / 2
        "circle"    -> pi * width * width
        _           -> 0

-- where 구문
calculateAreaWhere :: String -> Double -> Double -> Double
calculateAreaWhere shape width height = computeArea
    where
        computeArea
          | shape == "rectangle" = rectArea
          | shape == "triangle" = triArea
          | shape == "circle" = circleArea
          | otherwise = 0
        rectArea = width * height
        triArea = width * height / 2
        circleArea = pi * width * width

-- let in 구문
calculateAreaLetIn :: String -> Double -> Double -> Double
calculateAreaLetIn shape width height =
    let rectArea = width * height
        triArea = width * height / 2
        circleArea = pi * width * width
    in
        if shape == "rectangle" then rectArea
        else if shape == "triangle" then triArea
        else if shape == "circle" then circleArea
        else 0


main :: IO ()
main = do
    print $ calculateAreaCase "rectangle" 3 10
    print $ calculateAreaCase "triangle" 3 10
    print $ calculateAreaCase "circle" 3 10

    print $ calculateAreaWhere "rectangle" 3 10
    print $ calculateAreaWhere "triangle" 3 10
    print $ calculateAreaWhere "circle" 3 10

    print $ calculateAreaLetIn "rectangle" 3 10
    print $ calculateAreaLetIn "triangle" 3 10
    print $ calculateAreaLetIn "circle" 3 10

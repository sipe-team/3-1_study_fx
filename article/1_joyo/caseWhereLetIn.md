# Haskell의 case, where, let-in 구문 비교

## 선택 기준:

1. `case`: 패턴 매칭이 주된 목적일 때 사용
2. `where`: 여러 보조 계산이 함수 전체에서 재사용될 때 사용
3. `let-in`: 지역적인 계산이 필요하고 순차적인 계산이 자연스러울 때 사용


## 1. 원소의 크기 분류 함수

### case 구문

```haskell
classifyNumber :: Int -> String
classifyNumber num = case num of
    n | n < 0     -> "Negative"
      | n == 0    -> "Zero"
      | n < 10    -> "Small"
      | n < 100   -> "Medium"
      | otherwise -> "Large"
```

### where 구문

```haskell
classifyNumber :: Int -> String
classifyNumber num
    | isNegative = "Negative"
    | isZero     = "Zero"
    | isSmall    = "Small"
    | isMedium   = "Medium"
    | otherwise  = "Large"
    where
        isNegative = num < 0
        isZero     = num == 0
        isSmall    = num < 10
        isMedium   = num < 100
```

### let-in 구문

```haskell
classifyNumber :: Int -> String
classifyNumber num =
    let isNegative = num < 0
        isZero     = num == 0
        isSmall    = num < 10
        isMedium   = num < 100
    in if isNegative then "Negative"
       else if isZero then "Zero"
       else if isSmall then "Small"
       else if isMedium then "Medium"
       else "Large"
```

## 2. 점수 계산 함수

### case 구문

```haskell
calculateGrade :: Int -> Int -> String
calculateGrade midterm final =
    case (midterm + final) `div` 2 of
        score | score >= 90 -> "A"
              | score >= 80 -> "B"
              | score >= 70 -> "C"
              | score >= 60 -> "D"
              | otherwise   -> "F"
```

### where 구문

```haskell
calculateGrade :: Int -> Int -> String
calculateGrade midterm final
    | score >= 90 = "A"
    | score >= 80 = "B"
    | score >= 70 = "C"
    | score >= 60 = "D"
    | otherwise   = "F"
    where
        score = (midterm + final) `div` 2
```

### let-in 구문

```haskell
calculateGrade :: Int -> Int -> String
calculateGrade midterm final =
    let score = (midterm + final) `div` 2
    in  if score >= 90 then "A"
        else if score >= 80 then "B"
        else if score >= 70 then "C"
        else if score >= 60 then "D"
        else "F"
```

## 3. 도형 면적 계산 함수

### case 구문

```haskell
calculateArea :: String -> Double -> Double -> Double
calculateArea shape width height =
    case shape of
        "rectangle" -> width * height
        "triangle"  -> width * height / 2
        "circle"    -> pi * width * width
        _          -> 0
```

### where 구문

```haskell
calculateArea :: String -> Double -> Double -> Double
calculateArea shape width height = computeArea
    where
        computeArea
            | shape == "rectangle" = rectArea
            | shape == "triangle"  = triArea
            | shape == "circle"    = circArea
            | otherwise           = 0
        rectArea = width * height
        triArea  = width * height / 2
        circArea = pi * width * width
```

### let-in 구문

```haskell
calculateArea :: String -> Double -> Double -> Double
calculateArea shape width height =
    let rectArea = width * height
        triArea  = width * height / 2
        circArea = pi * width * width
    in  if shape == "rectangle" then rectArea
        else if shape == "triangle" then triArea
        else if shape == "circle" then circArea
        else 0
```
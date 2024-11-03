# 함수 정의를 위한 가드 문법

# Haskell Guard 문법

## 기본 구조

Guard 문법의 기본 형태는 다음과 같습니다:

```haskell
functionName pattern
  | condition1 = result1
  | condition2 = result2
  | otherwise  = defaultResult
```

## 기본 예제

### 1. 간단한 숫자 분류

```haskell
classifyNumber :: Int -> String
classifyNumber n
  | n < 0     = "음수"
  | n == 0    = "영"
  | otherwise = "양수"
```

### 2. where 절과 함께 사용

```haskell
bmi :: Double -> Double -> String
bmi weight height
  | value <= 18.5 = "저체중"
  | value <= 23.0 = "정상"
  | value <= 25.0 = "과체중"
  | otherwise     = "비만"
  where value = weight / (height * height)
```

## 고급 예제

### 1. 패턴 매칭과 Guard 조합

```haskell
calculateSalary :: String -> Int -> Double
calculateSalary "Engineer" years
  | years < 3  = 45000.0
  | years < 5  = 60000.0
  | otherwise  = 80000.0
calculateSalary "Manager" years
  | years < 5  = 65000.0
  | otherwise  = 100000.0
calculateSalary _ _ = 35000.0  -- 기본 급여
```

### 2. let과 Guard 조합

```haskell
circleProperties :: Double -> (Double, Double)
circleProperties r
  | r < 0     = (0, 0)
  | otherwise = let area = pi * r * r
                    circumference = 2 * pi * r
                in (area, circumference)
```

## 주요 특징

- 위에서 아래로 순차적으로 조건 평가
- 불리언 표현식만 사용 가능
- `where` 절이나 `let`을 통한 지역 변수 정의 가능
- 패턴 매칭과 함께 사용 가능

## 모범 사례

1. **가독성 중시**
   
   ```haskell
   getGrade :: Int -> Char
   getGrade score
     | score >= 90 = 'A'
     | score >= 80 = 'B'
     | score >= 70 = 'C'
     | otherwise   = 'F'
   ```

2. **복잡한 조건의 분리**
   
   ```haskell
   isValidDate :: Int -> Int -> Int -> Bool
   isValidDate year month day
     | month < 1 || month > 12 = False
     | day < 1 || day > daysInMonth = False
     | otherwise = True
     where daysInMonth = case month of
             2 | isLeapYear -> 29
               | otherwise  -> 28
             4  -> 30
             6  -> 30
             9  -> 30
             11 -> 30
             _  -> 31
           isLeapYear = year `mod` 4 == 0 && (year `mod` 100 /= 0 || year `mod` 400 == 0)
   ```

## 주의사항

- 모든 경우를 처리하도록 `otherwise` 사용 권장
- 조건의 순서가 중요함
- 각 guard는 반드시 불리언 결과를 반환해야 함
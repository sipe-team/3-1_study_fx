# 일반 함수와 람다 함수 비교

## 1. 기본 형태 비교

### 단일 매개변수 함수

```haskell
-- 일반 함수
double :: Int -> Int
double x = x * 2

-- 람다 함수
double :: Int -> Int
double = \x -> x * 2

-- 직접 사용
map double [1,2,3]          -- 일반 함수 사용
map (\x -> x * 2) [1,2,3]   -- 람다 함수 사용
```

### 여러 매개변수 함수

```haskell
-- 일반 함수
add :: Int -> Int -> Int
add x y = x + y

-- 람다 함수
add :: Int -> Int -> Int
add = \x y -> x + y

-- 또는
add = \x -> \y -> x + y
```

## 2. 패턴 매칭

### 리스트 패턴 매칭

```haskell
-- 일반 함수
head' :: [a] -> a
head' []     = error "Empty list"
head' (x:_)  = x

-- 람다 함수
head' :: [a] -> a
head' = \list -> case list of
    []    -> error "Empty list"
    (x:_) -> x
```

### 튜플 패턴 매칭

```haskell
-- 일반 함수
addPair :: (Int, Int) -> Int
addPair (x, y) = x + y

-- 람다 함수
addPair :: (Int, Int) -> Int
addPair = \(x, y) -> x + y
```

## 3. 고차 함수에서의 사용

### map 함수와 함께 사용

```haskell
-- 일반 함수 정의 후 사용
square :: Int -> Int
square x = x * x
squareList = map square [1,2,3]

-- 람다 함수 직접 사용
squareList = map (\x -> x * x) [1,2,3]
```

### filter 함수와 함께 사용

```haskell
-- 일반 함수
isEven :: Int -> Bool
isEven x = x `mod` 2 == 0
evenNumbers = filter isEven [1..10]

-- 람다 함수
evenNumbers = filter (\x -> x `mod` 2 == 0) [1..10]
```

## 4. 복잡한 예시

### 중첩된 계산

```haskell
-- 일반 함수
processNumbers :: Int -> Int -> Int
processNumbers x y =
    let doubled = x * 2
        tripled = y * 3
    in doubled + tripled

-- 람다 함수
processNumbers :: Int -> Int -> Int
processNumbers = \x y ->
    let doubled = x * 2
        tripled = y * 3
    in doubled + tripled
```

### 조건문 사용

```haskell
-- 일반 함수
checkValue :: Int -> String
checkValue x
    | x < 0     = "Negative"
    | x > 0     = "Positive"
    | otherwise = "Zero"

-- 람다 함수
checkValue :: Int -> String
checkValue = \x ->
    if x < 0 then "Negative"
    else if x > 0 then "Positive"
    else "Zero"
```

## 5. 주요 차이점

1. 가독성

```haskell
-- 일반 함수: 더 읽기 쉬움
sumSquares :: Int -> Int -> Int
sumSquares x y = x*x + y*y

-- 람다 함수: 더 복잡해 보일 수 있음
sumSquares :: Int -> Int -> Int
sumSquares = \x y -> x*x + y*y
```

2. 재사용성

```haskell
-- 일반 함수: 재사용하기 좋음
multiply :: Int -> Int -> Int
multiply x y = x * y

-- 람다 함수: 주로 일회성 사용에 적합
map (\x -> x * 3) [1,2,3]
```

3. 패턴 매칭의 편의성

```haskell
-- 일반 함수: 패턴 매칭이 자연스러움
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- 람다 함수: 패턴 매칭이 덜 직관적
factorial = \n -> case n of
    0 -> 1
    n -> n * factorial (n - 1)
```

## 6. 사용 가이드라인

### 일반 함수를 사용하는 경우:

- 여러 번 재사용되는 함수
- 복잡한 패턴 매칭이 필요한 경우
- 가독성이 중요한 경우
- 재귀 함수를 작성할 때

### 람다 함수를 사용하는 경우:

- 일회성 함수가 필요할 때
- 고차 함수에 인자로 전달할 때
- 간단한 변환이나 계산을 할 때
- 함수를 즉석에서 정의해야 할 때

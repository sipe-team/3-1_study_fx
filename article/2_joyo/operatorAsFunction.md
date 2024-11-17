# Haskell의 연산자 함수화 (Operator as Functions)

## 1. 기본 개념

중위 연산자를 괄호로 감싸면 일반 함수처럼 사용할 수 있습니다.

### 기본 문법

```haskell
(+)  :: Num a => a -> a -> a
(==) :: Eq a => a -> a -> Bool
(*)  :: Num a => a -> a -> a
(-)  :: Num a => a -> a -> a
```

## 2. 주요 연산자 함수들

### 산술 연산자

```haskell
-- 일반적인 중위 표기법
3 + 4        -- 7
5 * 6        -- 30

-- 함수로서의 사용
(+) 3 4      -- 7
(*) 5 6      -- 30

-- 고차 함수에서의 활용
map (+) [1,2,3] [4,5,6]  -- [5,7,9]
foldr (*) 1 [1,2,3,4]    -- 24
```

### 비교 연산자

```haskell
-- 일반적인 중위 표기법
5 == 5       -- True
4 /= 3       -- True

-- 함수로서의 사용
(==) 5 5     -- True
(/=) 4 3     -- True

-- 고차 함수에서의 활용
filter (== 3) [1,2,3,4,3] -- [3,3]
```

## 3. Section과의 차이점

### 완전한 함수 vs 부분 적용

```haskell
-- 완전한 이항 함수
(+)  :: Num a => a -> a -> a

-- Section (부분 적용)
(5+) :: Num a => a -> a
(+5) :: Num a => a -> a
```

### 사용 예시

```haskell
-- 완전한 함수로 사용
map (*) [1,2] [3,4]    -- [3,8]

-- Section으로 사용
map (*2) [1,2,3]       -- [2,4,6]
map (2*) [1,2,3]       -- [2,4,6]
```

## 4. 실용적인 예시

### 리스트 처리

```haskell
-- 숫자 리스트 더하기
zipWith (+) [1,2,3] [4,5,6]  -- [5,7,9]

-- 곱하기 테이블 만들기
map (*) [1..3]
-- [(*1),(*2),(*3)]  -- 각각 함수를 반환

-- 동등성 검사
any (== 5) [1,2,3,4,5]  -- True
```

### 함수형 스타일 활용

```haskell
-- fold 연산
sum = foldr (+) 0
product = foldr (*) 1

-- 필터링
removeZeros = filter (/= 0)
findEquals x = filter (== x)
```

## 5. 특별한 경우들

### 뺄셈 연산자의 특이점

```haskell
-- (-) 는 특별한 처리 필요
sub = (-)      -- 정상 동작
minus = (-5)   -- 에러! -5로 해석됨
subtract 5     -- 올바른 방법
```

### 비교 연산자 체인

```haskell
-- 일반적인 표현
1 <= x && x <= 10

-- 함수형 표현
and . zipWith (<=) [1] . (: [10])
```

## 6. 고급 활용

### 함수 합성에서의 활용

```haskell
-- 연산자들의 합성
sumSquares = foldr (+) 0 . map (^2)

-- 필터와 맵의 조합
processNumbers = filter (/= 0) . map (*2)
```

### 사용자 정의 연산자와 함께 사용

```haskell
-- 사용자 정의 연산자
(♦) :: Int -> Int -> Int
x ♦ y = x * y + x

-- 함수로 사용
(♦) 3 4       -- 15
map (♦ 2) [1,2,3]  -- [4,6,8]
```

## 7. 주의사항

### 가독성 고려

```haskell
-- 때로는 중위 표기가 더 읽기 쉬움
x + y         -- 더 명확함
(+) x y       -- 덜 명확함

-- 고차 함수에서는 함수 형태가 더 자연스러움
foldr (+) 0   -- 더 자연스러움
foldr x `plus` y  -- 덜 자연스러움
```

### 타입 추론

```haskell
-- 때로는 타입 명시가 필요
(+) :: Double -> Double -> Double
(*) :: Integer -> Integer -> Integer
```

연산자를 함수로 사용하는 것은 Haskell의 중요한 기능 중 하나이며, 특히 고차 함수와 함께 사용할 때 매우 유용합니다.
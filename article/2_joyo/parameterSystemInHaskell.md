# Haskell 함수의 매개변수 시스템

## 1. 커링(Currying)의 기본 개념

모든 Haskell 함수는 기술적으로는 단일 매개변수만 받지만, 커링을 통해 여러 매개변수를 가진 것처럼 동작합니다.

### 타입 시그니처 해석

```haskell
-- 일반적인 형태
add :: Int -> Int -> Int

-- 실제 해석
add :: Int -> (Int -> Int)
-- "Int를 받아서 (Int를 받아 Int를 반환하는 함수)를 반환하는 함수"
```

## 2. 다중 매개변수 함수의 실제 동작

### 기본 예시

```haskell
-- 두 정수를 더하는 함수
add :: Int -> Int -> Int
add x y = x + y

-- 다음 두 호출은 동일
result1 = add 3 4        -- 7
result2 = (add 3) 4      -- 7
```

### 단계별 평가

```haskell
add 3 4
-- 1단계: (add 3)가 새로운 함수 생성
-- 2단계: (add 3) 4 실행
```

## 3. 부분 적용(Partial Application)

커링의 강력한 특징은 함수의 부분 적용이 가능하다는 것입니다:

```haskell
-- 부분 적용 예시
add5 :: Int -> Int
add5 = add 5

-- 사용
result = add5 3  -- 8

-- 다른 예시
multiply :: Int -> Int -> Int
multiply x y = x * y

double = multiply 2
triple = multiply 3

result1 = double 4  -- 8
result2 = triple 4  -- 12
```

## 4. 실제 다중 매개변수 예시

### 세 개의 매개변수

```haskell
-- 3D 좌표의 거리 계산
distance :: Float -> Float -> Float -> Float
distance x y z = sqrt (x*x + y*y + z*z)

-- 다음 호출들은 모두 동일
d1 = distance 3 4 5
d2 = ((distance 3) 4) 5
d3 = (distance 3 4) 5
```

### 리스트 처리 함수

```haskell
-- 리스트의 특정 범위 추출
slice :: Int -> Int -> [a] -> [a]
slice start end lst = take (end - start) (drop start lst)

-- 부분 적용 활용
firstThree = slice 0 3
result = firstThree [1..10]  -- [1,2,3]
```

## 5. 튜플을 통한 다중 매개변수

때로는 여러 매개변수를 튜플로 그룹화하여 사용할 수도 있습니다:

```haskell
-- 커링 버전
addCurried :: Int -> Int -> Int
addCurried x y = x + y

-- 튜플 버전
addTupled :: (Int, Int) -> Int
addTupled (x, y) = x + y

-- 사용 비교
result1 = addCurried 3 4    -- 7
result2 = addTupled (3, 4)  -- 7
```

## 6. 실용적인 예시

### 고차 함수에서의 활용

```haskell
-- map과 부분 적용
numbers = [1..5]
result = map (multiply 2) numbers  -- [2,4,6,8,10]

-- filter와 부분 적용
isGreaterThan :: Int -> Int -> Bool
isGreaterThan x y = y > x

greaterThan5 = filter (isGreaterThan 5)
result = greaterThan5 [1..10]  -- [6,7,8,9,10]
```

### 함수 조합

```haskell
-- 문자열 처리 함수
processString :: Int -> String -> String -> String
processString n prefix str = prefix ++ take n str

-- 부분 적용으로 새로운 함수 생성
firstFiveWithPrefix = processString 5 "Result: "
result = firstFiveWithPrefix "Hello, World!"  -- "Result: Hello"
```

## 정리

1. Haskell의 모든 함수는 기술적으로는 단일 매개변수 함수
2. 커링을 통해 다중 매개변수처럼 동작
3. 부분 적용을 통해 유연한 함수 생성 가능
4. 필요한 경우 튜플을 사용하여 다중 매개변수 그룹화 가능

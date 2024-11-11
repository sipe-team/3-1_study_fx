# Haskell의 커링(Currying)

## 1. 커링의 정의

커링은 다중 매개변수를 받는 함수를 단일 매개변수 함수들의 체인으로 변환하는 과정입니다.

### 기본 문법과 타입
```haskell
-- 일반적으로 보이는 형태
add :: Int -> Int -> Int
add x y = x + y

-- 실제 내부 동작 형태
add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)
```

## 2. 커링의 동작 방식

### 단계별 평가 과정
```haskell
add 3 4

-- 단계 1: add 3
(\x -> (\y -> x + y)) 3
= \y -> 3 + y     -- 새로운 함수 생성

-- 단계 2: (\y -> 3 + y) 4
= 3 + 4
= 7
```

### 괄호를 명시적으로 표현
```haskell
-- 다음 표현들은 모두 동일
add 3 4
(add 3) 4
((add) 3) 4
```

## 3. 커링의 실제 응용

### 부분 적용(Partial Application)
```haskell
-- 기본 함수
multiply :: Int -> Int -> Int -> Int
multiply x y z = x * y * z

-- 부분 적용 예시
double = multiply 2
doubleAndTriple = multiply 2 3

-- 사용
result1 = double 3 4      -- 2 * 3 * 4 = 24
result2 = doubleAndTriple 4  -- 2 * 3 * 4 = 24
```

### 함수 파이프라인 구성
```haskell
-- 문자열 처리 파이프라인
processString :: Int -> String -> String -> String
processString len prefix str = prefix ++ take len str

-- 부분 적용으로 새로운 함수 생성
addHeader = processString 10 "Header: "
addFooter = processString 10 "Footer: "

-- 사용
result1 = addHeader "Hello World"  -- "Header: Hello Worl"
result2 = addFooter "Hello World"  -- "Footer: Hello Worl"
```

## 4. 고급 커링 패턴

### 중첩된 커링
```haskell
-- 3차원 좌표 변환
transform :: Float -> (Float -> (Float -> (Float, Float, Float)))
transform x y z = (x * 2, y * 2, z * 2)

-- 다음과 같이 사용 가능
point1 = transform 1 2 3       -- (2,4,6)
point2 = (transform 1) 2 3     -- (2,4,6)
point3 = ((transform 1) 2) 3   -- (2,4,6)
```

### 섹션(Section)과 커링
```haskell
-- 연산자의 부분 적용
add10 = (+ 10)    -- 왼쪽 섹션
divideBy2 = (/ 2) -- 왼쪽 섹션
multiplyBy3 = (3 *) -- 오른쪽 섹션

-- 사용
result1 = add10 5        -- 15
result2 = divideBy2 10   -- 5
result3 = multiplyBy3 4  -- 12
```

## 5. 실전 활용 예시

### 필터링과 매핑
```haskell
-- 리스트 처리 함수
filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap pred transform list = map transform (filter pred list)

-- 부분 적용 활용
processEvens = filterMap even (*2)
processPositives = filterMap (>0) abs

-- 사용
result1 = processEvens [1..5]      -- [4,8]
result2 = processPositives [-2..2]  -- [2,1,1,2]
```

### 설정 가능한 함수 생성
```haskell
-- 로그 메시지 생성기
makeLogger :: String -> String -> String -> String
makeLogger level prefix message = 
    "[" ++ level ++ "] " ++ prefix ++ ": " ++ message

-- 부분 적용으로 특정 로거 생성
errorLogger = makeLogger "ERROR" "System"
warnLogger = makeLogger "WARN" "User"

-- 사용
log1 = errorLogger "Disk full"  -- "[ERROR] System: Disk full"
log2 = warnLogger "Invalid input"  -- "[WARN] User: Invalid input"
```

## 6. 커링의 장점

1. 유연성
   - 함수의 일부 매개변수만 적용 가능
   - 재사용 가능한 특수화된 함수 생성

2. 모듈성
   - 복잡한 함수를 작은 단위로 분해
   - 함수 조합이 용이

3. 가독성
   - 의도가 명확한 새로운 함수 생성
   - 코드의 추상화 수준 조절 가능

## 7. 주의사항

### 과도한 커링 피하기
```haskell
-- 피해야 할 패턴
overlyCurried = ((((func a) b) c) d) e

-- 더 나은 방식
betterStyle = func a b c d e
```

### 타입 명시의 중요성
```haskell
-- 복잡한 커링에서는 타입 시그니처를 명확히
process :: Int -> (String -> (Bool -> String))
process n s b = if b then replicate n s else ""
```

커링은 Haskell의 핵심 개념 중 하나로, 함수형 프로그래밍의 강력한 도구입니다. 적절히 활용하면 더 유연하고 재사용 가능한 코드를 작성할 수 있습니다.
# Haskell의 Sections(섹션)

## 1. Section의 정의

섹션은 중위 연산자(infix operator)를 부분적으로 적용한 함수입니다. 연산자의 한 쪽 인자를 고정하여 새로운 함수를 만듭니다.

### 기본 문법
```haskell
-- 왼쪽 섹션
(x+)   -- \y -> x + y

-- 오른쪽 섹션
(+y)   -- \x -> x + y

-- 일반 괄호와 비교
(+)    -- 이항 연산자를 함수로 변환
```

## 2. Section의 종류

### 왼쪽 섹션 (Left Section)
```haskell
-- 첫 번째 인자를 고정
(5+)    -- \y -> 5 + y
(2*)    -- \y -> 2 * y
("Hello"++)  -- \y -> "Hello" ++ y

-- 사용 예시
map (10*) [1,2,3]    -- [10,20,30]
filter (5<) [1..10]  -- [6,7,8,9,10]
```

### 오른쪽 섹션 (Right Section)
```haskell
-- 두 번째 인자를 고정
(+5)    -- \x -> x + 5
(*2)    -- \x -> x * 2
(++[1,2])  -- \x -> x ++ [1,2]

-- 사용 예시
map (+1) [1,2,3]     -- [2,3,4]
filter (>5) [1..10]  -- [6,7,8,9,10]
```

## 3. 실용적인 활용

### 리스트 처리
```haskell
-- 리스트의 모든 원소에 2를 곱하기
map (*2) [1..5]      -- [2,4,6,8,10]

-- 5보다 큰 수 찾기
filter (>5) [1..10]  -- [6,7,8,9,10]

-- 각 리스트에 특정 원소 추가
map (1:) [[2,3], [4,5]]  -- [[1,2,3], [1,4,5]]
```

### 문자열 처리
```haskell
-- 접두사 추가
map ("Mr. "++) ["Smith", "Jones"]  -- ["Mr. Smith", "Mr. Jones"]

-- 접미사 추가
map (++"!") ["Hello", "Hi"]        -- ["Hello!", "Hi!"]
```

## 4. 특별한 경우들

### 감산 연산자의 특이점
```haskell
-- 주의: (-5)는 섹션이 아닌 음수를 의미
(-5)    -- 음수 5
(subtract 5)  -- \x -> x - 5  (오른쪽 섹션의 대안)
(5-)    -- \x -> 5 - x  (왼쪽 섹션은 정상 동작)
```

### 비교 연산자
```haskell
-- 크기 비교
(>5)    -- 5보다 큰 수
(<10)   -- 10보다 작은 수
(>=0)   -- 0보다 크거나 같은 수

-- 동등성 비교
(==0)   -- 0과 같은 수
(/=100) -- 100과 다른 수
```

## 5. 고급 활용 예시

### 함수형 파이프라인 구축
```haskell
-- 데이터 처리 파이프라인
processNumbers = filter (>0) . map (*2) . filter even

-- 사용
processNumbers [1..10]  -- [4,8,12,16,20]
```

### 복합 조건문 작성
```haskell
-- 여러 조건 결합
multipleOf = \n -> (==0) . (`mod` n)
isMultipleOf5 = multipleOf 5

filter isMultipleOf5 [1..20]  -- [5,10,15,20]
```

## 6. 실전 패턴

### 데이터 변환
```haskell
-- 점수 처리
adjustScores = map (*1.1)    -- 모든 점수를 10% 증가
failingScores = filter (<60)  -- 60점 미만 점수 찾기

-- 사용
adjustScores [55,65,75]      -- [60.5,71.5,82.5]
failingScores [55,65,75]     -- [55]
```

### 텍스트 처리
```haskell
-- 문자열 포맷팅
addPrefix = map ("log: "++)
addSuffix = map (++" end")

-- 사용
addPrefix ["error", "warning"]  -- ["log: error", "log: warning"]
addSuffix ["start", "middle"]   -- ["start end", "middle end"]
```

## 7. 주의사항과 모범 사례

### 가독성 고려
```haskell
-- 좋은 예시
filter (>0) numbers
map (*2) positives

-- 피해야 할 예시 (너무 복잡함)
filter (&&) . map (>0) . zipWith (<) xs
```

### 명확한 이름 사용
```haskell
-- 의미 있는 이름으로 섹션 래핑
isPositive = (>0)
double = (*2)
addGreeting = ("Hello "++)

-- 사용
filter isPositive [0,1,-2,3]  -- [1,3]
map double [1,2,3]           -- [2,4,6]
map addGreeting ["John", "Jane"]  -- ["Hello John", "Hello Jane"]
```

섹션은 Haskell에서 함수의 부분 적용을 간단하고 우아하게 표현할 수 있게 해주는 강력한 기능입니다.
# Haskell의 Infix(중위) 연산자

## 1. Infix의 정의

Infix는 연산자가 피연산자 사이에 위치하는 표기법입니다.

### 기본 형태

```haskell
-- Infix 표기
4 + 3
5 * 2
"hello" ++ "world"

-- 같은 표현의 Prefix 표기
(+) 4 3
(*) 5 2
(++) "hello" "world"
```

## 2. Infix 연산자의 종류

### 기본 산술 연산자

```haskell
3 + 2    -- 덧셈
4 - 1    -- 뺄셈
5 * 2    -- 곱셈
6 / 3    -- 나눗셈
5 `div` 2  -- 정수 나눗셈
```

### 리스트 연산자

```haskell
[1,2] ++ [3,4]  -- 리스트 연결
1 : [2,3]       -- 원소 추가
"hello" !! 1    -- 인덱스 접근
```

### 논리 연산자

```haskell
True && False   -- 논리곱
True || False   -- 논리합
5 == 5          -- 동등 비교
3 /= 4          -- 불일치 비교
4 >= 3          -- 크거나 같음
```

## 3. 사용자 정의 Infix 연산자

### 연산자 정의

```haskell
-- 새로운 연산자 정의
infixl 6 +++  -- 왼쪽 결합성, 우선순위 6
(+++) :: Int -> Int -> Int
a +++ b = a + 2 * b

-- 사용
5 +++ 3  -- 결과: 11
```

### 함수를 Infix로 사용

```haskell
-- 일반 함수를 백틱(`)으로 감싸서 infix로 사용
divide x y = x `div` y
maximum x y = x `max` y

-- 예시
10 `divide` 2  -- 5
3 `max` 7      -- 7
```

## 4. 결합성과 우선순위

### 결합성 지정

```haskell
infixl -- 왼쪽 결합
infixr -- 오른쪽 결합
infix  -- 결합성 없음

-- 예시
infixl 6 +     -- 왼쪽 결합, 우선순위 6
infixr 5 ++    -- 오른쪽 결합, 우선순위 5
infix 4 ==     -- 결합성 없음, 우선순위 4
```

### 우선순위 레벨

```haskell
-- 일반적인 우선순위
-- 높은 숫자가 높은 우선순위
infixl 7 *, /
infixl 6 +, -
infixr 5 ++
infix 4 ==, /=, <, <=, >=, >
```

## 5. 실용적인 예시

### 사용자 정의 연산자

```haskell
-- 벡터 연산자
data Vector = Vector Int Int

infixl 6 <+>  -- 벡터 덧셈
(<+>) :: Vector -> Vector -> Vector
(Vector x1 y1) <+> (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)

-- 사용
Vector 1 2 <+> Vector 3 4  -- Vector 4 6
```

### 함수형 연산자

```haskell
-- 함수 합성
infixr 9 .
(f . g) x = f (g x)

-- 함수 적용
infixr 0 $
f $ x = f x

-- 사용
map (+1) . filter even $ [1..10]
```

## 6. 주의사항

### 가독성

```haskell
-- 좋은 예
x `elem` list
list1 ++ list2

-- 피해야 할 예
x~~~y>>>z  -- 의미가 불분명한 연산자
```

### 우선순위 관리

```haskell
-- 명시적 괄호 사용이 좋을 때
(a + b) * c
map f (filter p xs)
```

## 7. 모범 사례

### 일반적인 패턴

```haskell
-- 데이터 처리
numbers `intersect` primes

-- 수학적 연산
3 `pow` 2

-- 텍스트 처리
"Hello" `append` "World"
```

### 실용적 사용

```haskell
-- 읽기 쉬운 코드
users `filterBy` isActive `sortBy` name

-- 데이터 변환
rawData `convertTo` JSON `writeFile` "output.json"
```

Infix 표기법은 코드의 가독성을 높이고 자연스러운 표현을 가능하게 하는 Haskell의 강력한 기능입니다.

# Haskell의 결합성: infixl과 infixr

## 1. 결합성의 기본 개념

### 정의
- `infixl`: 왼쪽 결합성 (left associative)
- `infixr`: 오른쪽 결합성 (right associative)
- `infix`: 결합성 없음 (no associativity)

### 문법
```haskell
infixl n op  -- 왼쪽 결합, n은 우선순위(0-9)
infixr n op  -- 오른쪽 결합, n은 우선순위(0-9)
infix n op   -- 결합성 없음, n은 우선순위(0-9)
```

## 2. 왼쪽 결합성 (infixl)

### 동작 방식
```haskell
-- infixl로 정의된 연산자 ⊕
a ⊕ b ⊕ c ⊕ d
-- 다음과 같이 해석됨:
((a ⊕ b) ⊕ c) ⊕ d
```

### 실제 예시
```haskell
-- 덧셈과 뺄셈은 왼쪽 결합
infixl 6 +, -

15 - 5 - 3
-- 해석: (15 - 5) - 3
-- = 10 - 3
-- = 7

-- 만약 오른쪽 결합이었다면:
-- 15 - (5 - 3)
-- = 15 - 2
-- = 13
```

## 3. 오른쪽 결합성 (infixr)

### 동작 방식
```haskell
-- infixr로 정의된 연산자 ⊗
a ⊗ b ⊗ c ⊗ d
-- 다음과 같이 해석됨:
a ⊗ (b ⊗ (c ⊗ d))
```

### 실제 예시
```haskell
-- 리스트 연결 연산자는 오른쪽 결합
infixr 5 ++

[1,2] ++ [3,4] ++ [5,6]
-- 해석: [1,2] ++ ([3,4] ++ [5,6])
-- = [1,2] ++ [3,4,5,6]
-- = [1,2,3,4,5,6]

-- cons 연산자도 오른쪽 결합
infixr 5 :
1 : 2 : 3 : []
-- 해석: 1 : (2 : (3 : []))
-- = [1,2,3]
```

## 4. 결합성의 중요성

### 다른 결과가 나오는 경우
```haskell
-- 지수 연산에서의 차이
infixl 8 ^   -- 왼쪽 결합
2 ^ 3 ^ 2
-- = (2 ^ 3) ^ 2
-- = 8 ^ 2
-- = 64

infixr 8 ^   -- 오른쪽 결합
2 ^ 3 ^ 2
-- = 2 ^ (3 ^ 2)
-- = 2 ^ 9
-- = 512
```

### 함수 합성에서의 결합성
```haskell
-- 함수 합성 연산자는 오른쪽 결합
infixr 9 .

f . g . h
-- 해석: f . (g . h)
-- = f . (g . h)
-- ≠ (f . g) . h
```

## 5. 실용적인 예시들

### 문자열 연산
```haskell
-- 문자열 연결
infixr 5 ++
"Hello" ++ " " ++ "World" ++ "!"
-- = "Hello" ++ (" " ++ ("World" ++ "!"))
-- = "Hello World!"

-- 사용자 정의 문자열 연산자
infixl 5 +++ 
(+++) :: String -> String -> String
s1 +++ s2 = s1 ++ " " ++ s2

"Hello" +++ "World" +++ "Again"
-- = ("Hello" +++ "World") +++ "Again"
-- = "Hello World Again"
```

### 수학 연산
```haskell
-- 곱셈은 왼쪽 결합
infixl 7 *
2 * 3 * 4
-- = (2 * 3) * 4
-- = 6 * 4
-- = 24

-- 사용자 정의 수학 연산자
infixr 8 ***
(***) :: Num a => a -> a -> a
x *** y = x * (y + 1)

2 *** 3 *** 4
-- = 2 *** (3 *** 4)
-- = 2 *** (3 * (4 + 1))
-- = 2 *** 15
-- = 2 * (15 + 1)
-- = 32
```

## 6. 주의사항과 팁

### 우선순위와 결합성 함께 고려하기
```haskell
-- 우선순위와 결합성 모두 중요
infixl 7 *
infixl 6 +

# Haskell의 결합성: infixl과 infixr

## 1. 결합성의 기본 개념

### 정의
- `infixl`: 왼쪽 결합성 (left associative)
- `infixr`: 오른쪽 결합성 (right associative)
- `infix`: 결합성 없음 (no associativity)

### 문법
```haskell
infixl n op  -- 왼쪽 결합, n은 우선순위(0-9)
infixr n op  -- 오른쪽 결합, n은 우선순위(0-9)
infix n op   -- 결합성 없음, n은 우선순위(0-9)
```

## 2. 왼쪽 결합성 (infixl)

### 동작 방식
```haskell
-- infixl로 정의된 연산자 ⊕
a ⊕ b ⊕ c ⊕ d
-- 다음과 같이 해석됨:
((a ⊕ b) ⊕ c) ⊕ d
```

### 실제 예시
```haskell
-- 덧셈과 뺄셈은 왼쪽 결합
infixl 6 +, -

15 - 5 - 3
-- 해석: (15 - 5) - 3
-- = 10 - 3
-- = 7

-- 만약 오른쪽 결합이었다면:
-- 15 - (5 - 3)
-- = 15 - 2
-- = 13
```

## 3. 오른쪽 결합성 (infixr)

### 동작 방식
```haskell
-- infixr로 정의된 연산자 ⊗
a ⊗ b ⊗ c ⊗ d
-- 다음과 같이 해석됨:
a ⊗ (b ⊗ (c ⊗ d))
```

### 실제 예시
```haskell
-- 리스트 연결 연산자는 오른쪽 결합
infixr 5 ++

[1,2] ++ [3,4] ++ [5,6]
-- 해석: [1,2] ++ ([3,4] ++ [5,6])
-- = [1,2] ++ [3,4,5,6]
-- = [1,2,3,4,5,6]

-- cons 연산자도 오른쪽 결합
infixr 5 :
1 : 2 : 3 : []
-- 해석: 1 : (2 : (3 : []))
-- = [1,2,3]
```

## 4. 결합성의 중요성

### 다른 결과가 나오는 경우
```haskell
-- 지수 연산에서의 차이
infixl 8 ^   -- 왼쪽 결합
2 ^ 3 ^ 2
-- = (2 ^ 3) ^ 2
-- = 8 ^ 2
-- = 64

infixr 8 ^   -- 오른쪽 결합
2 ^ 3 ^ 2
-- = 2 ^ (3 ^ 2)
-- = 2 ^ 9
-- = 512
```

### 함수 합성에서의 결합성
```haskell
-- 함수 합성 연산자는 오른쪽 결합
infixr 9 .

f . g . h
-- 해석: f . (g . h)
-- = f . (g . h)
-- ≠ (f . g) . h
```

## 5. 실용적인 예시들

### 문자열 연산
```haskell
-- 문자열 연결
infixr 5 ++
"Hello" ++ " " ++ "World" ++ "!"
-- = "Hello" ++ (" " ++ ("World" ++ "!"))
-- = "Hello World!"

-- 사용자 정의 문자열 연산자
infixl 5 +++ 
(+++) :: String -> String -> String
s1 +++ s2 = s1 ++ " " ++ s2

"Hello" +++ "World" +++ "Again"
-- = ("Hello" +++ "World") +++ "Again"
-- = "Hello World Again"
```

### 수학 연산
```haskell
-- 곱셈은 왼쪽 결합
infixl 7 *
2 * 3 * 4
-- = (2 * 3) * 4
-- = 6 * 4
-- = 24

-- 사용자 정의 수학 연산자
infixr 8 ***
(***) :: Num a => a -> a -> a
x *** y = x * (y + 1)

2 *** 3 *** 4
-- = 2 *** (3 *** 4)
-- = 2 *** (3 * (4 + 1))
-- = 2 *** 15
-- = 2 * (15 + 1)
-- = 32
```

## 6. 주의사항과 팁

### 우선순위와 결합성 함께 고려하기
```haskell
-- 우선순위와 결합성 모두 중요
infixl 7 *
infixl 6 +

2 + 3 * 4 + 5
-- 우선순위: * > +
-- = 2 + (3 * 4) + 5
-- 결합성: 왼쪽
-- = (2 + (3 * 4)) + 5
-- = (2 + 12) + 5
-- = 14 + 5
-- = 19
```

### 명시적 괄호 사용
```haskell
-- 복잡한 표현식에서는 괄호 사용 권장
(2 + 3) * (4 + 5)  -- 명확함
2 + 3 * 4 + 5      -- 덜 명확함
```

결합성을 잘 이해하고 적절히 사용하면 더 명확하고 효율적인 코드를 작성할 수 있습니다.2 + 3 * 4 + 5
-- 우선순위: * > +
-- = 2 + (3 * 4) + 5
-- 결합성: 왼쪽
-- = (2 + (3 * 4)) + 5
-- = (2 + 12) + 5
-- = 14 + 5
-- = 19
```

### 명시적 괄호 사용
```haskell
-- 복잡한 표현식에서는 괄호 사용 권장
(2 + 3) * (4 + 5)  -- 명확함
2 + 3 * 4 + 5      -- 덜 명확함
```

결합성을 잘 이해하고 적절히 사용하면 더 명확하고 효율적인 코드를 작성할 수 있습니다.
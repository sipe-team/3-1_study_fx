# Haskell의 리스트(List) 타입

## 1. 기본 문법과 특징

### 1.1 리스트 정의

```haskell
-- 기본 문법
numbers = [1, 2, 3, 4, 5]          -- 숫자 리스트
chars = ['a', 'b', 'c']            -- 문자 리스트
strings = ["apple", "banana"]       -- 문자열 리스트
nested = [[1,2], [3,4], [5,6]]     -- 중첩 리스트

-- 타입 선언
numbers :: [Int]
chars :: [Char]     -- String과 동일
strings :: [String]
nested :: [[Int]]
```

### 1.2 리스트 생성 방법

```haskell
-- 직접 나열
list1 = [1, 2, 3, 4, 5]

-- 범위 표현
list2 = [1..5]        -- [1,2,3,4,5]
list3 = ['a'..'e']    -- "abcde"
list4 = [2,4..10]     -- [2,4,6,8,10]
list5 = [5,4..1]      -- [5,4,3,2,1]

-- 무한 리스트
list6 = [1..]         -- [1,2,3,...]
list7 = cycle [1,2,3] -- [1,2,3,1,2,3,...]
list8 = repeat 5      -- [5,5,5,...]
```

### 1.3 리스트 생성자 (List Constructor)

- 리스트는 두개의 constructor 를 가진다. [] and :
- Empty List:

  ```haskell
    [] :: [a]
  ```

- Add ahead:

  ```haskell
    : :: a -> [a] -> [a]
  ```

- example:

  ```haskell
    1 : [2,3,4]
    -- [1,2,3,4]

    -- The notation [15, 12, 21] is a shorthand for 15 : 12 : 21 : []
    -- which means 15 : (12 : (21 : []))
  ```

## 2. 리스트 연산

### 2.1 기본 연산자

```haskell
-- (:) cons 연산자
1 : [2,3,4]     -- [1,2,3,4]

-- (++) 연결 연산자
[1,2] ++ [3,4]  -- [1,2,3,4]

-- (!!) 인덱싱
[1,2,3] !! 1    -- 2

-- null 빈 리스트 확인
null []         -- True
null [1,2]      -- False
```

### 2.2 기본 함수들

```haskell
-- 리스트 조작
head [1,2,3]    -- 1
tail [1,2,3]    -- [2,3]
init [1,2,3]    -- [1,2]
last [1,2,3]    -- 3

-- 리스트 정보
length [1,2,3]  -- 3
sum [1,2,3]     -- 6
product [1,2,3] -- 6
```

```haskell
-- sum 함수는 리스트의 모든 원소를 더한다.
sum [1,2,3]     -- 6

-- sum 함수의 패턴 정의
sum [] = 0 -- base case
sum (x:xs) = x + sum xs -- recursive case

-- (x:xs) 패턴은 리스트의 첫번째 원소를 x에 바인딩하고 나머지 리스트를 xs에 바인딩한다.
```

## 3. 리스트 컴프리헨션

### 3.1 기본 문법

```haskell
-- 기본 형태
squares = [x^2 | x <- [1..5]]  -- [1,4,9,16,25]

-- 조건 추가
evens = [x | x <- [1..10], even x]  -- [2,4,6,8,10]

-- 다중 생성자
pairs = [(x,y) | x <- [1,2], y <- ['a','b']]
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

-- 중첩 조건
matrix = [[x,y] | x <- [1..2], y <- [1..2], x /= y]
-- [[1,2],[2,1]]
```

### 3.2 실제 응용

```haskell
-- 소수 찾기
primes = [x | x <- [2..], not $ any (\n -> x `mod` n == 0) [2..x-1]]

-- 피타고라스 삼각형
pythTriples = [(a,b,c) |
    c <- [1..10],
    b <- [1..c],
    a <- [1..b],
    a^2 + b^2 == c^2]
```

## 4. 패턴 매칭과 리스트

### 4.1 기본 패턴 매칭

```haskell
-- 빈 리스트와 비어있지 않은 리스트
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

-- head와 tail 패턴
firstElement :: [a] -> Maybe a
firstElement []     = Nothing
firstElement (x:_)  = Just x

-- 여러 원소 패턴
describe :: [a] -> String
describe []  = "Empty"
describe [x] = "One element"
describe [x,y] = "Two elements"
describe (x:xs) = "More than two elements"
```

### 4.2 재귀적 패턴 매칭

```haskell
-- 리스트 길이 계산
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

-- 리스트 합계 계산
mySum :: Num a => [a] -> a
mySum []     = 0
mySum (x:xs) = x + mySum xs
```

## 5. 고차 함수와 리스트

### 5.1 map과 filter

```haskell
-- map 예시
doubled = map (*2) [1..5]        -- [2,4,6,8,10]
squared = map (^2) [1..5]        -- [1,4,9,16,25]

-- filter 예시
evens = filter even [1..10]      -- [2,4,6,8,10]
positives = filter (>0) [-2..2]  -- [1,2]
```

### 5.2 fold 함수들

```haskell
-- foldr (오른쪽에서 왼쪽으로)
sum' = foldr (+) 0              -- 합계 계산
product' = foldr (*) 1          -- 곱 계산
concat' = foldr (++) []         -- 리스트 연결

-- foldl (왼쪽에서 오른쪽으로)
sum'' = foldl (+) 0             -- 합계 계산
reverse' = foldl (flip (:)) []  -- 리스트 뒤집기
```

## 6. 리스트 성능 고려사항

### 6.1 장점

```haskell
-- 머리에 추가 (O(1))
newList = 1 : oldList

-- 패턴 매칭 효율성
case list of
    []     -> "Empty"
    (x:xs) -> "Not empty"
```

### 6.2 주의사항

```haskell
-- 끝에 추가 (O(n)) - 피해야 함
slowAppend = oldList ++ [newElement]

-- 인덱스 접근 (O(n)) - 피해야 함
element = list !! 1000

-- 대신 다음과 같이 사용
betterAccess = head (drop 1000 list)
```

## 7. 실제 응용 예시

````haskell
-- 리스트 처리 함수 조합
processNumbers :: [Int] -> [Int]
processNumbers = filter (>0)       -- 양수만 선택# Haskell의 리스트(List) 타입

## 1. 기본 문법과 특징

### 1.1 리스트 정의
```haskell
-- 기본 문법
numbers = [1, 2, 3, 4, 5]          -- 숫자 리스트
chars = ['a', 'b', 'c']            -- 문자 리스트
strings = ["apple", "banana"]       -- 문자열 리스트
nested = [[1,2], [3,4], [5,6]]     -- 중첩 리스트

-- 타입 선언
numbers :: [Int]
chars :: [Char]     -- String과 동일
strings :: [String]
nested :: [[Int]]
````

### 1.2 리스트 생성 방법

```haskell
-- 직접 나열
list1 = [1, 2, 3, 4, 5]

-- 범위 표현
list2 = [1..5]        -- [1,2,3,4,5]
list3 = ['a'..'e']    -- "abcde"
list4 = [2,4..10]     -- [2,4,6,8,10]
list5 = [5,4..1]      -- [5,4,3,2,1]

-- 무한 리스트
list6 = [1..]         -- [1,2,3,...]
list7 = cycle [1,2,3] -- [1,2,3,1,2,3,...]
list8 = repeat 5      -- [5,5,5,...]
```

## 2. 리스트 연산

### 2.1 기본 연산자

```haskell
-- (:) cons 연산자
1 : [2,3,4]     -- [1,2,3,4]

-- (++) 연결 연산자
[1,2] ++ [3,4]  -- [1,2,3,4]

-- (!!) 인덱싱
[1,2,3] !! 1    -- 2

-- null 빈 리스트 확인
null []         -- True
null [1,2]      -- False
```

### 2.2 기본 함수들

```haskell
-- 리스트 조작
head [1,2,3]    -- 1
tail [1,2,3]    -- [2,3]
init [1,2,3]    -- [1,2]
last [1,2,3]    -- 3

-- 리스트 정보
length [1,2,3]  -- 3
sum [1,2,3]     -- 6
product [1,2,3] -- 6
```

## 3. 리스트 컴프리헨션

### 3.1 기본 문법

```haskell
-- 기본 형태
squares = [x^2 | x <- [1..5]]  -- [1,4,9,16,25]

-- 조건 추가
evens = [x | x <- [1..10], even x]  -- [2,4,6,8,10]

-- 다중 생성자
pairs = [(x,y) | x <- [1,2], y <- ['a','b']]
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

-- 중첩 조건
matrix = [[x,y] | x <- [1..2], y <- [1..2], x /= y]
-- [[1,2],[2,1]]
```

### 3.2 실제 응용

```haskell
-- 소수 찾기
primes = [x | x <- [2..], not $ any (\n -> x `mod` n == 0) [2..x-1]]

-- 피타고라스 삼각형
pythTriples = [(a,b,c) |
    c <- [1..10],
    b <- [1..c],
    a <- [1..b],
    a^2 + b^2 == c^2]
```

## 4. 패턴 매칭과 리스트

### 4.1 기본 패턴 매칭

```haskell
-- 빈 리스트와 비어있지 않은 리스트
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

-- head와 tail 패턴
firstElement :: [a] -> Maybe a
firstElement []     = Nothing
firstElement (x:_)  = Just x

-- 여러 원소 패턴
describe :: [a] -> String
describe []  = "Empty"
describe [x] = "One element"
describe [x,y] = "Two elements"
describe (x:xs) = "More than two elements"
```

### 4.2 재귀적 패턴 매칭

```haskell
-- 리스트 길이 계산
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

-- 리스트 합계 계산
mySum :: Num a => [a] -> a
mySum []     = 0
mySum (x:xs) = x + mySum xs
```

## 5. 고차 함수와 리스트

### 5.1 map과 filter

```haskell
-- map 예시
doubled = map (*2) [1..5]        -- [2,4,6,8,10]
squared = map (^2) [1..5]        -- [1,4,9,16,25]

-- filter 예시
evens = filter even [1..10]      -- [2,4,6,8,10]
positives = filter (>0) [-2..2]  -- [1,2]
```

### 5.2 fold 함수들

```haskell
-- foldr (오른쪽에서 왼쪽으로)
sum' = foldr (+) 0              -- 합계 계산
product' = foldr (*) 1          -- 곱 계산
concat' = foldr (++) []         -- 리스트 연결

-- foldl (왼쪽에서 오른쪽으로)
sum'' = foldl (+) 0             -- 합계 계산
reverse' = foldl (flip (:)) []  -- 리스트 뒤집기
```

## 6. 리스트 성능 고려사항

### 6.1 장점

```haskell
-- 머리에 추가 (O(1))
newList = 1 : oldList

-- 패턴 매칭 효율성
case list of
    []     -> "Empty"
    (x:xs) -> "Not empty"
```

### 6.2 주의사항

```haskell
-- 끝에 추가 (O(n)) - 피해야 함
slowAppend = oldList ++ [newElement]

-- 인덱스 접근 (O(n)) - 피해야 함
element = list !! 1000

-- 대신 다음과 같이 사용
betterAccess = head (drop 1000 list)
```

## 7. 실제 응용 예시

# Haskell의 리스트(List) 타입

## 1. 기본 문법과 특징

### 1.1 리스트 정의

```haskell
-- 기본 문법
numbers = [1, 2, 3, 4, 5]          -- 숫자 리스트
chars = ['a', 'b', 'c']            -- 문자 리스트
strings = ["apple", "banana"]       -- 문자열 리스트
nested = [[1,2], [3,4], [5,6]]     -- 중첩 리스트

-- 타입 선언
numbers :: [Int]
chars :: [Char]     -- String과 동일
strings :: [String]
nested :: [[Int]]
```

### 1.2 리스트 생성 방법

```haskell
-- 직접 나열
list1 = [1, 2, 3, 4, 5]

-- 범위 표현
list2 = [1..5]        -- [1,2,3,4,5]
list3 = ['a'..'e']    -- "abcde"
list4 = [2,4..10]     -- [2,4,6,8,10]
list5 = [5,4..1]      -- [5,4,3,2,1]

-- 무한 리스트
list6 = [1..]         -- [1,2,3,...]
list7 = cycle [1,2,3] -- [1,2,3,1,2,3,...]
list8 = repeat 5      -- [5,5,5,...]
```

## 2. 리스트 연산

### 2.1 기본 연산자

```haskell
-- (:) cons 연산자
1 : [2,3,4]     -- [1,2,3,4]

-- (++) 연결 연산자
[1,2] ++ [3,4]  -- [1,2,3,4]

-- (!!) 인덱싱
[1,2,3] !! 1    -- 2

-- null 빈 리스트 확인
null []         -- True
null [1,2]      -- False
```

### 2.2 기본 함수들

```haskell
-- 리스트 조작
head [1,2,3]    -- 1
tail [1,2,3]    -- [2,3]
init [1,2,3]    -- [1,2]
last [1,2,3]    -- 3

-- 리스트 정보
length [1,2,3]  -- 3
sum [1,2,3]     -- 6
product [1,2,3] -- 6
```

## 3. 리스트 컴프리헨션

### 3.1 기본 문법

```haskell
-- 기본 형태
squares = [x^2 | x <- [1..5]]  -- [1,4,9,16,25]

-- 조건 추가
evens = [x | x <- [1..10], even x]  -- [2,4,6,8,10]

-- 다중 생성자
pairs = [(x,y) | x <- [1,2], y <- ['a','b']]
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

-- 중첩 조건
matrix = [[x,y] | x <- [1..2], y <- [1..2], x /= y]
-- [[1,2],[2,1]]
```

### 3.2 실제 응용

```haskell
-- 소수 찾기
primes = [x | x <- [2..], not $ any (\n -> x `mod` n == 0) [2..x-1]]

-- 피타고라스 삼각형
pythTriples = [(a,b,c) |
    c <- [1..10],
    b <- [1..c],
    a <- [1..b],
    a^2 + b^2 == c^2]
```

## 4. 패턴 매칭과 리스트

### 4.1 기본 패턴 매칭

```haskell
-- 빈 리스트와 비어있지 않은 리스트
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

-- head와 tail 패턴
firstElement :: [a] -> Maybe a
firstElement []     = Nothing
firstElement (x:_)  = Just x

-- 여러 원소 패턴
describe :: [a] -> String
describe []  = "Empty"
describe [x] = "One element"
describe [x,y] = "Two elements"
describe (x:xs) = "More than two elements"
```

### 4.2 재귀적 패턴 매칭

```haskell
-- 리스트 길이 계산
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

-- 리스트 합계 계산
mySum :: Num a => [a] -> a
mySum []     = 0
mySum (x:xs) = x + mySum xs
```

## 5. 고차 함수와 리스트

### 5.1 map과 filter

```haskell
-- map 예시
doubled = map (*2) [1..5]        -- [2,4,6,8,10]
squared = map (^2) [1..5]        -- [1,4,9,16,25]

-- filter 예시
evens = filter even [1..10]      -- [2,4,6,8,10]
positives = filter (>0) [-2..2]  -- [1,2]
```

### 5.2 fold 함수들

```haskell
-- foldr (오른쪽에서 왼쪽으로)
sum' = foldr (+) 0              -- 합계 계산
product' = foldr (*) 1          -- 곱 계산
concat' = foldr (++) []         -- 리스트 연결

-- foldl (왼쪽에서 오른쪽으로)
sum'' = foldl (+) 0             -- 합계 계산
reverse' = foldl (flip (:)) []  -- 리스트 뒤집기
```

## 6. 리스트 성능 고려사항

### 6.1 장점

```haskell
-- 머리에 추가 (O(1))
newList = 1 : oldList

-- 패턴 매칭 효율성
case list of
    []     -> "Empty"
    (x:xs) -> "Not empty"
```

### 6.2 주의사항

```haskell
-- 끝에 추가 (O(n)) - 피해야 함
slowAppend = oldList ++ [newElement]

-- 인덱스 접근 (O(n)) - 피해야 함
element = list !! 1000

-- 대신 다음과 같이 사용
betterAccess = head (drop 1000 list)
```

## 7. 실제 응용 예시

```````haskell
-- 리스트 처리 함수 조합
processNumbers :: [Int] -> [Int]
processNumbers = filter (>0)       -- 양수만 선택
               . map (*2)          -- 2배
               . filter even       -- 짝수만 선택

-- 데이터 변환
data Person = Person { name :: String, age :: Int }

processPersons :: [Person] -> [(String, Int)]
processPersons = map (\p -> (name p, age p))
               . filter (\p -> age p >= 18)

-- 복잡한 리스트 연산
matrix :: [[Int]] -> [[Int]] -> [[Int]]
matrix a b = [[ sum $ zipWith (*) row col | col <- transpose b] | row <- a]
``````haskell
-- 리스트 처리 함수 조합
processNumbers :: [Int] -> [Int]
processNumbers = filter (>0)       -- 양수만 선택
               . map (*2)          -- 2배
               . filter even       -- 짝수만 선택

-- 데이터 변환
data Person = Person { name :: String, age :: Int }

processPersons :: [Person] -> [(String, Int)]
processPersons = map (\p -> (name p, age p))
               . filter (\p -> age p >= 18)

-- 복잡한 리스트 연산
matrix :: [[Int]] -> [[Int]] -> [[Int]]
matrix a b = [[ sum $ zipWith (*) row col | col <- transpose b] | row <- a]
```````

               . map (*2)          -- 2배
               . filter even       -- 짝수만 선택

-- 데이터 변환
data Person = Person { name :: String, age :: Int }

processPersons :: [Person] -> [(String, Int)]
processPersons = map (\p -> (name p, age p))
. filter (\p -> age p >= 18)

-- 복잡한 리스트 연산
matrix :: [[Int]] -> [[Int]] -> [[Int]]
matrix a b = [[ sum $ zipWith (*) row col | col <- transpose b] | row <- a]

```

```

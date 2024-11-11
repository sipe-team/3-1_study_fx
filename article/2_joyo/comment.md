# Haskell의 주석 시스템

## 1. 주석의 기본 유형

### 한 줄 주석 (Single-line Comments)

```haskell
-- 이것은 한 줄 주석입니다
x = 5 -- 변수에 대한 설명

-- 여러 줄을 각각 주석 처리
-- 첫 번째 줄
-- 두 번째 줄
-- 세 번째 줄
```

### 여러 줄 주석 (Multi-line Comments)

```haskell
{-
이것은 여러 줄 주석입니다.
여러 줄에 걸쳐
주석을 작성할 수 있습니다.
-}

sum x y = x + y {- 두 수를 더하는 함수 -}
```

## 2. 중첩된 주석

### 중첩 가능한 여러 줄 주석

```haskell
{- 외부 주석
   {- 내부 주석 -}
   여전히 외부 주석
-}

{- 레벨 1
   {- 레벨 2
      {- 레벨 3 -}
   -}
-}
```

## 3. 문서화 주석 (Haddock)

### 모듈 문서화

```haskell
{-|
Module      : DataStructures
Description : 기본 데이터 구조 구현
Copyright   : (c) John Doe, 2024
License     : BSD3
Maintainer  : johndoe@example.com

자세한 모듈 설명을 여기에 작성합니다.
-}
module DataStructures where
```

### 함수 문서화

```haskell
-- | 두 정수를 더합니다.
add :: Int -> Int -> Int
add x y = x + y

{-|
리스트의 모든 요소의 합을 계산합니다.
예시:
>>> sumList [1,2,3]
6
-}
sumList :: Num a => [a] -> a
sumList = foldr (+) 0
```

## 4. 주석 사용의 모범 사례

### 함수 설명

```haskell
-- | 피보나치 수열의 n번째 값을 계산합니다.
-- 예시:
--   >>> fib 5
--   5
--   >>> fib 6
--   8
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
```

### 데이터 타입 설명

```haskell
-- | 사용자 정보를 저장하는 데이터 타입
data User = User {
    -- | 사용자 고유 ID
    userId   :: Int,
    -- | 사용자 이름
    userName :: String,
    -- | 사용자 이메일
    userEmail:: String
    } deriving (Show, Eq)
```

## 5. 실용적인 주석 패턴

### 구현 설명

```haskell
{- | 이진 검색 트리 구현
노드는 다음 속성을 가집니다:
1. 왼쪽 서브트리의 모든 노드 < 현재 노드
2. 오른쪽 서브트리의 모든 노드 > 현재 노드
-}
data BSTree a = Empty
              | Node a (BSTree a) (BSTree a)
              deriving (Show)

-- | 트리에 새 값을 삽입
-- O(log n) 시간 복잡도
insert :: Ord a => a -> BSTree a -> BSTree a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
    | x < y     = Node y (insert x left) right
    | otherwise = Node y left (insert x right)
```

### 알고리즘 설명

```haskell
{- | 퀵정렬 구현
   분할 정복 방식을 사용:
   1. 피벗 선택 (첫 번째 원소)
   2. 피벗보다 작은/큰 원소로 분할
   3. 재귀적으로 각 부분 정렬
   4. 결과 결합
-}
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    quicksort smaller ++ [x] ++ quicksort larger
    where
        smaller = [y | y <- xs, y <= x]  -- 피벗보다 작거나 같은 원소
        larger  = [y | y <- xs, y > x]   -- 피벗보다 큰 원소
```

## 6. 문서화 모범 사례

### 모듈 레벨 문서화

```haskell
{-|
Module      : Graphics.Shape
Description : 2D 도형 연산을 위한 기본 도형 정의
License     : GPL-3

이 모듈은 기본적인 2D 도형과 관련 연산을 제공합니다.
주요 기능:
  * 도형 생성
  * 면적 계산
  * 둘레 계산
  * 도형 변환
-}
```

### 복잡한 함수 문서화

```haskell
{-| 행렬 곱셈 구현
입력 행렬 A(m×n)와 B(n×p)에 대해 C(m×p) 계산

제약 조건:
  * 첫 번째 행렬의 열 수 = 두 번째 행렬의 행 수
  * 빈 행렬 입력 시 빈 행렬 반환

예시:
>>> matrixMult [[1,2],[3,4]] [[5,6],[7,8]]
[[19,22],[43,50]]

복잡도: O(m×n×p)
-}
matrixMult :: Num a => [[a]] -> [[a]] -> [[a]]
```

주석은 코드의 가독성과 유지보수성을 높이는 중요한 도구입니다. 하지만 너무 많은 주석은 오히려 코드를 읽기 어렵게 만들 수 있으므로, 적절한 균형을 유지하는 것이 중요합니다.

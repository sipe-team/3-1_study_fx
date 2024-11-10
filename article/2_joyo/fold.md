# Haskell의 Fold 함수들

## 1. Fold 함수의 종류

Haskell은 세 가지 주요 fold 함수를 제공합니다:
- `foldr`: 오른쪽에서 왼쪽으로 fold (fold right)
- `foldl`: 왼쪽에서 오른쪽으로 fold (fold left)
- `foldl'`: strict한 왼쪽 fold (엄격한 평가)

### 타입 시그니처
```haskell
foldr  :: (a -> b -> b) -> b -> [a] -> b
foldl  :: (b -> a -> b) -> b -> [a] -> b
foldl' :: (b -> a -> b) -> b -> [a] -> b
```

## 2. foldr (Fold Right)

### 기본 동작
```haskell
foldr f z [x1, x2, x3] = x1 `f` (x2 `f` (x3 `f` z))

-- 예시: 리스트 합계
foldr (+) 0 [1,2,3]
= 1 + (2 + (3 + 0))
= 1 + (2 + 3)
= 1 + 5
= 6
```

### 주요 사용 예시
```haskell
-- 리스트 연결
concat :: [[a]] -> [a]
concat = foldr (++) []

-- 리스트 생성
map f = foldr (\x acc -> f x : acc) []

-- 필터링
filter p = foldr (\x acc -> if p x then x:acc else acc) []
```

## 3. foldl (Fold Left)

### 기본 동작
```haskell
foldl f z [x1, x2, x3] = ((z `f` x1) `f` x2) `f` x3

-- 예시: 리스트 합계
foldl (+) 0 [1,2,3]
= ((0 + 1) + 2) + 3
= (1 + 2) + 3
= 3 + 3
= 6
```

### 주요 사용 예시
```haskell
-- 리스트 뒤집기
reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

-- 평균 계산
average :: [Double] -> Double
average xs = foldl (\acc x -> acc + x) 0 xs / fromIntegral (length xs)
```

## 4. foldl' (Strict Fold Left)

### 특징
- 메모리 효율적
- 큰 리스트 처리에 적합
- 지연 평가 대신 즉시 평가

### 사용 예시
```haskell
import Data.List (foldl')

-- 큰 숫자의 합계
sumBig :: Num a => [a] -> a
sumBig = foldl' (+) 0

-- 문자열 연결
concatStrings :: [String] -> String
concatStrings = foldl' (++) ""
```

## 5. 각 Fold의 특징 비교

### foldr의 장점
- 무한 리스트 처리 가능
- 지연 평가에 적합
- 리스트 구조 유지에 유용

```haskell
-- 무한 리스트에서 처음 5개 요소 가져오기
take 5 $ foldr (:) [] [1..]  -- [1,2,3,4,5]
```

### foldl의 특징
- 왼쪽에서 오른쪽으로 처리
- 누적 값을 먼저 계산
- 큰 리스트에서는 메모리 문제 발생 가능

```haskell
-- 주의: 큰 리스트에서는 권장되지 않음
foldl (+) 0 [1..1000000]
```

### foldl'의 용도
- 수치 계산에 적합
- 메모리 효율적
- 큰 리스트 처리에 권장

```haskell
-- 권장되는 사용법
foldl' (+) 0 [1..1000000]
```

## 6. 실용적인 예시

### 데이터 변환
```haskell
-- 리스트를 Map으로 변환
import qualified Data.Map as Map
listToMap :: [(String, Int)] -> Map.Map String Int
listToMap = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty
```

### 조건부 처리
```haskell
-- 모든 조건 만족 검사
all :: (a -> Bool) -> [a] -> Bool
all p = foldr (\x acc -> p x && acc) True

-- 어느 하나라도 조건 만족 검사
any :: (a -> Bool) -> [a] -> Bool
any p = foldr (\x acc -> p x || acc) False
```

### 문자열 처리
```haskell
-- 단어 뒤집기
reverseWords :: String -> String
reverseWords = unwords . foldr (\w acc -> reverse w : acc) [] . words
```

## 7. 성능 고려사항

### 언제 어떤 fold를 사용할까?

1. foldr 사용
   - 리스트를 구성할 때
   - 무한 리스트 처리할 때
   - 지연 평가가 필요할 때

2. foldl' 사용
   - 수치 계산할 때
   - 큰 리스트 처리할 때
   - 즉시 평가가 필요할 때

3. foldl 사용 (거의 사용하지 않음)
   - 특별한 경우가 아니면 foldl' 선호

```haskell
-- Good: foldr for list construction
map f = foldr (\x acc -> f x : acc) []

-- Good: foldl' for numeric computation
sum = foldl' (+) 0

-- Bad: foldl for large lists
sum = foldl (+) 0  -- 메모리 문제 발생 가능
```

fold 함수들은 Haskell 프로그래밍의 핵심 도구이며, 상황에 맞는 적절한 fold를 선택하는 것이 중요합니다.
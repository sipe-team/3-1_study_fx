# Haskell의 iterate 함수

## 1. iterate의 정의

### 타입 시그니처

```haskell
iterate :: (a -> a) -> a -> [a]
```

### 기본 동작

- 함수(f)와 초기값(x)을 받아 무한 리스트를 생성
- 결과: [x, f x, f (f x), f (f (f x)), ...]

### 기본 구현

```haskell
iterate f x = x : iterate f (f x)
```

## 2. 기본 사용 예시

### 숫자 시퀀스 생성

```haskell
-- 2의 거듭제곱
iterate (*2) 1
-- [1,2,4,8,16,32,64,...]

-- 3씩 증가
iterate (+3) 0
-- [0,3,6,9,12,15,...]

-- take로 제한하기
take 5 $ iterate (*2) 1
-- [1,2,4,8,16]
```

### 함수 반복 적용

```haskell
-- 문자열에 "!" 추가
take 3 $ iterate (++"!") "Hi"
-- ["Hi","Hi!","Hi!!"]

-- 리스트 길이 두 배로
take 4 $ iterate (++[1,2]) [1,2]
-- [[1,2],[1,2,1,2],[1,2,1,2,1,2],[1,2,1,2,1,2,1,2]]
```

## 3. 실용적인 예시

### 피보나치 수열

```haskell
-- 튜플을 사용한 피보나치
fibs = map fst $ iterate (\(a,b) -> (b,a+b)) (0,1)
take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]
```

### 기하학적 수열

```haskell
-- 각 원소가 이전 원소의 2배
geometricSequence = iterate (*2) 1
take 5 geometricSequence
-- [1,2,4,8,16]

-- 각 원소가 이전 원소의 1/2
halves = iterate (/2) 1
take 5 halves
-- [1.0,0.5,0.25,0.125,0.0625]
```

### 리스트 변환

```haskell
-- 각 단계마다 리스트의 모든 원소에 1 더하기
incrementLists = iterate (map (+1)) [1,2,3]
take 3 incrementLists
-- [[1,2,3],[2,3,4],[3,4,5]]

-- 각 단계마다 리스트 두 번 반복
repeatLists = iterate (concat . replicate 2) [1]
take 3 repeatLists
-- [[1],[1,1],[1,1,1,1]]
```

## 4. iterate와 다른 함수들의 조합

### iterate와 takeWhile

```haskell
-- 1000 미만의 2의 거듭제곱
takeWhile (<1000) $ iterate (*2) 1
-- [1,2,4,8,16,32,64,128,256,512]

-- 양수인 값들만
takeWhile (>0) $ iterate (\x -> x-3) 10
-- [10,7,4,1]
```

### iterate와 filter

```haskell
-- 짝수인 2의 거듭제곱만
take 5 $ filter even $ iterate (*2) 1
-- [2,4,8,16,32]

-- 5로 나누어 떨어지는 수만
take 3 $ filter (\x -> x `mod` 5 == 0) $ iterate (+1) 0
-- [0,5,10]
```

## 5. 고급 사용 예시

### 함수 합성으로 복잡한 변환

```haskell
-- 각 단계마다 제곱하고 1 더하기
take 5 $ iterate (\x -> x^2 + 1) 2
-- [2,5,26,677,458330]

-- 문자열 처리
take 3 $ iterate (reverse . map toUpper) "hello"
-- ["hello","OLLEH","hello"]
```

### 무한 데이터 구조

```haskell
-- 무한 이진 트리
data Tree a = Leaf a | Node (Tree a) (Tree a)

infiniteTree = iterate (\t -> Node t t) (Leaf 1)
```

## 6. 성능 고려사항

### 장점

- 지연 평가 덕분에 효율적
- 무한 시퀀스를 메모리 효율적으로 처리
- 필요한 만큼만 계산

### 주의사항

```haskell
-- 메모리 누수 가능성
-- Bad:
let x = last $ take 1000000 $ iterate (+1) 0

-- Good:
let x = foldr const 0 $ take 1000000 $ iterate (+1) 0
```

## 7. 일반적인 패턴

### 수학적 시퀀스

```haskell
-- 등차수열
arithmetic d = iterate (+d)

-- 등비수열
geometric r = iterate (*r)

-- 제곱수
squares = map (^2) $ iterate (+1) 0
```

### 상태 변환

```haskell
-- 게임 상태 진행
type GameState = (Int, String)
gameSteps = iterate updateGame initialState
  where updateGame (score, msg) = (score + 10, "Level " ++ show score)
```

iterate는 무한 시퀀스를 생성하는 강력한 도구이며, 필요한 만큼만 계산하는 Haskell의 지연 평가와 잘 어울립니다.

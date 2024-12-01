# Haskell의 함수 합성 연산자 (.)

## 1. 기본 정의

### 타입 시그니처
```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
```

### 함수 정의
```haskell
(f . g) x = f (g x)
-- 또는
f . g = \x -> f (g x)
```

## 2. 작동 방식 설명

### 기본 원리
- 두 함수를 합성하여 새로운 함수 생성
- 오른쪽 함수(g)를 먼저 실행한 후, 그 결과를 왼쪽 함수(f)에 적용
- 수학의 함수 합성(f ∘ g)과 동일한 개념

### 타입 흐름
```haskell
-- 입력 x의 타입이 a일 때:
x    -- 타입: a
g x  -- 타입: b
f (g x) -- 타입: c
```

### 간단한 예시로 이해하기
```haskell
-- 예시 1: 숫자를 문자열로 변환 후 길이 구하기
length . show $ 123
-- = length (show 123)
-- = length "123"
-- = 3

-- 예시 2: 리스트의 모든 원소를 2배로 만든 후 합계 구하기
sum . map (*2) $ [1,2,3]
-- = sum (map (*2) [1,2,3])
-- = sum [2,4,6]
-- = 12
```

## 3. 함수 합성의 특징

### 결합법칙
```haskell
(f . g) . h = f . (g . h)
```

### $ 연산자와의 조합
```haskell
f . g $ x  -- 함수 합성 후 적용
f $ g x    -- 함수 적용 순서 지정
```

### 포인트-프리 스타일
```haskell
-- 일반적인 스타일
f x = g (h x)

-- 함수 합성 사용
f = g . h
```

[이어서 실용적인 예시들...]

## 4. 실용적인 예시

### 문자열 처리
```haskell
-- 이메일 주소 정제하기
cleanEmail :: String -> String
cleanEmail = map toLower . filter isAlphaNum

-- "John.Doe@Example.com" -> "johndoeexamplecom"
```

### 리스트 처리
```haskell
-- 짝수만 골라서 제곱하기
processNumbers :: [Int] -> [Int]
processNumbers = map (^2) . filter even

-- [1,2,3,4,5,6] -> [4,16,36]
```

### 파일 처리
```haskell
-- 파일에서 고유한 단어 수 세기
countUniqueWords :: String -> Int
countUniqueWords = length . nub . words . map toLower

-- "Hello hello World" -> 2
```

### 데이터 변환
```haskell
-- 전화번호 포맷팅
formatPhoneNumber :: String -> String
formatPhoneNumber = concat . intersperse "-" . chunksOf 4 . filter isNumber

-- "01012345678" -> "010-1234-5678"
```

## 5. 함수 합성의 장점

1. 코드 간결성
   - 임시 변수 제거
   - 로직을 파이프라인으로 표현

2. 가독성
   - 데이터 흐름을 명확하게 표현
   - 각 변환 단계를 명확히 구분

3. 재사용성
   - 작은 함수들의 조합으로 새로운 기능 생성
   - 모듈화된 코드 작성 가능

4. 타입 안전성
   - 컴파일 시점에 타입 검사
   - 타입 불일치 오류 조기 발견

## 6. 주의사항

1. 타입 호환성
```haskell
-- 올바른 사용
length . show    -- show :: a -> String, length :: String -> Int

-- 잘못된 사용
show . length    -- 타입 불일치 에러
```

2. 과도한 사용 피하기
```haskell
-- 피해야 할 예
f . g . h . i . j . k  -- 너무 복잡함

-- 더 나은 방식
intermediate = h . i . j . k
result = f . g . intermediate
```

이러한 함수 합성은 Haskell 프로그래밍의 핵심 도구이며, 깔끔하고 유지보수가 쉬운 코드를 작성하는 데 매우 유용합니다.
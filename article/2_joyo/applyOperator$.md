# Haskell의 함수 적용 연산자 ($)

## 1. 기본 정의

### 타입 시그니처

```haskell
($) :: (a -> b) -> a -> b
```

### 함수 정의

```haskell
f $ x = f x
```

### 특징

- 우선순위가 가장 낮음 (0)
- 오른쪽 결합(right associative)
- 중위 연산자로 사용

## 2. 주요 용도: 괄호 제거

### 괄호 사용 vs ($) 사용

```haskell
-- 괄호 사용
show (1 + 2)
length (filter even [1..10])
sum (map (*2) (filter even [1..10]))

-- ($) 사용
show $ 1 + 2
length $ filter even [1..10]
sum $ map (*2) $ filter even [1..10]
```

## 3. 실용적인 예시

### 문자열 처리

```haskell
-- 괄호 사용
putStrLn (reverse (toUpper "hello"))

-- ($) 사용
putStrLn $ reverse $ toUpper "hello"

-- 결과: "OLLEH"
```

### 리스트 처리

```haskell
-- 괄호 사용
length (filter (<5) (map (*2) [1..10]))

-- ($) 사용
length $ filter (<5) $ map (*2) [1..10]

-- 결과: 2
```

### 파일 입출력

```haskell
-- 괄호 사용
writeFile "output.txt" (unlines (map show [1..10]))

-- ($) 사용
writeFile "output.txt" $ unlines $ map show [1..10]
```

### 텍스트 포맷팅

```haskell
-- 괄호 사용
putStrLn ("현재 값: " ++ show (sum (map (*2) [1..5])))

-- ($) 사용
putStrLn $ "현재 값: " ++ show $ sum $ map (*2) [1..5]

-- 결과: "현재 값: 30"
```

## 4. ($)의 다양한 활용

### 부분 적용

```haskell
-- map과 함께 사용
map ($ 3) [(+1), (*2), (^2)]
-- 결과: [4,6,9]

-- 함수 리스트에 적용
map ($"hello") [reverse, map toUpper, (++ "!")]
-- 결과: ["olleh","HELLO","hello!"]
```

### 함수 합성과 함께 사용

```haskell
-- (.) 와 ($) 조합
length . show $ 12345
map toUpper . reverse $ "hello"
```

## 5. 가독성 향상 예시

### 중첩된 함수 호출

```haskell
-- 읽기 어려운 버전
sum (filter even (map (^2) (take 10 [1..])))

-- ($) 사용으로 개선
sum $ filter even $ map (^2) $ take 10 [1..]
```

### JSON 파싱 예시

```haskell
-- 복잡한 중첩
decode (pack (readFile "data.json"))

-- ($) 사용
decode $ pack $ readFile "data.json"
```

## 6. 주의사항

### 과도한 사용 피하기

```haskell
-- 피해야 할 패턴
f $ g $ h $ i $ j $ k  -- 너무 많은 $ 사용

-- 더 나은 방식
let intermediate = h $ i $ j $ k
in f $ g $ intermediate
```

### 연산자 우선순위

```haskell
-- 잘못된 사용
length $ [1..10] ++ [11..20]  -- 의도하지 않은 동작

-- 올바른 사용
length $ ([1..10] ++ [11..20])
length ([1..10] ++ [11..20])  -- 괄호가 더 명확할 수 있음
```

## 7. 실전 활용 예시

### 웹 애플리케이션

```haskell
-- API 응답 처리
processResponse $ parseJSON $ getResponse $ makeRequest "api/data"
```

### 파일 처리

```haskell
-- 파일 내용 처리
writeFile "output.txt" $ unlines $ map processLine $ lines $ readFile "input.txt"
```

### 데이터 변환

```haskell
-- 사용자 입력 처리
saveUser $ validateInput $ sanitizeInput $ getUserInput
```

($) 연산자는 Haskell에서 코드를 더 읽기 쉽게 만들고 불필요한 괄호를 제거하는 데 매우 유용한 도구입니다. 적절히 사용하면 코드의 가독성을 크게 향상시킬 수 있습니다.

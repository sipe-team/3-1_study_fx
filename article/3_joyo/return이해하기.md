# Haskell의 return 이해하기
- "return은 일반 값을 모나드 컨텍스트로 감싸는 함수입니다."


## 1. return의 기본 개념

```haskell
-- return의 타입 시그니처
return :: Monad m => a -> m a

-- 의미: 값 a를 모나드 m의 컨텍스트로 감싸기
```

### 1.1 다양한 모나드에서의 return

```haskell
-- Maybe 모나드
return 5 :: Maybe Int    -- 결과: Just 5

-- IO 모나드
return "hello" :: IO String  -- 결과: IO 컨텍스트의 "hello"

-- List 모나드
return 3 :: [Int]       -- 결과: [3]
```

## 2. 실제 사용 예시

### 2.1 Maybe 모나드에서

```haskell
-- 계산 결과를 Maybe로 감싸기
calculateAge :: Int -> Maybe Int
calculateAge year =
    if year > 2024
        then Nothing
        else return (2024 - year)  -- Just (2024 - year)
```

### 2.2 IO 모나드에서

```haskell
-- 사용자 입력 처리
processInput :: IO Int
processInput = do
    input <- getLine
    let number = read input
    return (number * 2)  -- 결과를 IO 컨텍스트로 감싸기
```

## 3. return이 필요한 상황

### 3.1 do 표기법에서

```haskell
-- 모나드 연산의 마지막에서 결과 반환
getUserInfo :: IO (String, Int)
getUserInfo = do
    name <- getLine
    age <- readLn
    return (name, age)  -- 튜플을 IO 컨텍스트로 감싸기
```

### 3.2 계산 결과 감싸기

```haskell
-- 순수 계산 결과를 모나드 컨텍스트로 변환
safeDivide :: Int -> Int -> Maybe Int
safeDivide n d =
    if d == 0
        then Nothing
        else return (n `div` d)  -- 성공적인 나눗셈 결과를 Maybe로 감싸기
```

## 4. 일반적인 return과의 차이점

### 4.1 일반적인 프로그래밍 언어의 return

```python
# Python에서의 return
def function():
    return 5  # 함수 실행 종료, 5를 반환
```

### 4.2 Haskell의 return

```haskell
-- Haskell에서의 return
calculation :: IO Int
calculation = do
    x <- return 5    -- 5를 IO 컨텍스트로 감싸기
    return (x + 1)   -- (x + 1)을 IO 컨텍스트로 감싸기
```

## 5. 주요 사용 패턴

### 5.1 계산 결과 감싸기

```haskell
processData :: String -> Maybe String
processData str = do
    let processed = filter (/= ' ') str
    if null processed
        then Nothing
        else return processed  -- 처리된 문자열을 Maybe로 감싸기
```

### 5.2 모나드 체인의 마지막

```haskell
calculateTotal :: IO Int
calculateTotal = do
    x <- readLn
    y <- readLn
    return (x + y)  -- 합계를 IO 컨텍스트로 감싸기
```

## 6. 실용적인 예시

### 6.1 게임 상태 업데이트

```haskell
updateGameState :: GameState -> IO GameState
updateGameState state = do
    newScore <- calculateScore state
    if newScore > 100
        then return state { gameOver = True }  -- 수정된 상태를 IO로 감싸기
        else return state { score = newScore } -- 수정된 상태를 IO로 감싸기
```

### 6.2 데이터 검증

```haskell
validateUser :: User -> Maybe User
validateUser user = do
    if null (userName user)
        then Nothing
        else return user  -- 유효한 사용자를 Maybe로 감싸기
```

이해하기 쉽게 비유하자면:

- 일반적인 return: "이 함수를 여기서 끝내고 이 값을 반환해"
- Haskell의 return: "이 값을 특별한 상자(컨텍스트)에 담아줘"

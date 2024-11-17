# Control.Monad (when) 이해하기

## 1. when의 기본 개념

### 1.1 타입 시그니처
```haskell
when :: Monad m => Bool -> m () -> m ()
```
- 첫 번째 매개변수: 조건 (Bool)
- 두 번째 매개변수: 수행할 액션 (m ())
- 반환값: 액션의 결과 (m ())

### 1.2 when의 동작
```haskell
-- when의 내부 구현은 다음과 유사합니다
when :: Monad m => Bool -> m () -> m ()
when condition action = if condition
                        then action
                        else return ()
```

## 2. 일반적인 if문과의 비교

### 2.1 일반적인 if문 사용
```haskell
-- if문 사용
playAgain :: IO ()
playAgain = do
    choice <- getLine
    if choice == "y"
        then startGame
        else return ()
```

### 2.2 when 사용
```haskell
-- when 사용 (더 간결함)
playAgain :: IO ()
playAgain = do
    choice <- getLine
    when (choice == "y") startGame
```

## 3. 실제 사용 예시

### 3.1 기본 사용
```haskell
-- 게임 재시작 예시
main :: IO ()
main = do
    putStr "Play again? (y/n): "
    choice <- getLine
    when (choice == "y") playGame
```

### 3.2 여러 조건 조합
```haskell
-- 디버그 메시지 출력
debugLog :: Bool -> String -> IO ()
debugLog isDebug message = 
    when isDebug $ do
        putStrLn "DEBUG:"
        putStrLn message
        putStrLn "-----"
```

### 3.3 중첩 사용
```haskell
processData :: Bool -> [Int] -> IO ()
processData debug nums = do
    when debug $ putStrLn "Processing data..."
    when (not $ null nums) $ do
        putStrLn $ "First number: " ++ show (head nums)
        when debug $ putStrLn "First number processed"
```

## 4. when이 유용한 상황

### 4.1 조건부 로깅
```haskell
data LogLevel = Debug | Info | Error
type Logger = LogLevel -> String -> IO ()

logger :: Bool -> Logger
logger isVerbose level message = 
    when (isVerbose || level == Error) $ 
        putStrLn $ show level ++ ": " ++ message
```

### 4.2 게임 로직
```haskell
updateGameState :: GameState -> IO ()
updateGameState state = do
    when (score state > highScore state) $ do
        putStrLn "New High Score!"
        saveHighScore (score state)
    
    when (health state <= 0) $ do
        putStrLn "Game Over!"
        resetGame
```

### 4.3 사용자 입력 검증
```haskell
validateInput :: String -> IO ()
validateInput input = do
    when (null input) $
        putStrLn "Input cannot be empty"
    
    when (length input < 3) $
        putStrLn "Input too short"
```

## 5. 장점과 사용해야 할 때

### 5.1 장점
1. 코드 간결성
2. 가독성 향상
3. 단순한 조건부 실행에 적합

### 5.2 사용하면 좋은 경우
```haskell
-- 1. 단순한 조건부 실행
when debug $ putStrLn "Debug mode"

-- 2. 여러 액션을 하나의 블록으로
when shouldUpdate $ do
    updateDatabase
    notifyUser
    logChange

-- 3. 부수 효과가 있는 작업의 조건부 실행
when (userRole == Admin) $
    modifySystemSettings
```

## 6. 실용적인 예시들

### 6.1 파일 처리
```haskell
processFile :: FilePath -> Bool -> IO ()
processFile path verbose = do
    exists <- doesFileExist path
    when exists $ do
        content <- readFile path
        when verbose $
            putStrLn $ "File size: " ++ show (length content)
        processContent content
```

### 6.2 네트워크 요청
```haskell
makeRequest :: URL -> Bool -> IO Response
makeRequest url shouldRetry = do
    response <- httpGet url
    when (statusCode response >= 500 && shouldRetry) $ do
        putStrLn "Server error, retrying..."
        makeRequest url False
    return response
```

`when`은 조건부 실행을 간단하고 읽기 쉽게 만들어주는 유용한 함수입니다. 특히 IO 액션을 조건부로 실행할 때 매우 유용합니다.
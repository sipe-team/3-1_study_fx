# Haskell의 main :: IO () 이해하기

## 1. Main 함수란?
- 프로그램의 시작점을 나타내는 특별한 함수
- C, Java 등 다른 언어의 main 함수와 같은 역할
- 반드시 `main :: IO ()` 형태의 타입을 가져야 함

## 2. 문법 분석
```haskell
main :: IO ()
```
위 코드는 다음과 같이 해석됩니다:
- `main` → 함수 이름 
- `::` → "~의 타입은" (타입 선언 기호)
- `IO` → 입출력 작업을 수행할 수 있는 타입
- `()` → 반환값 없음 (void와 유사)

## 3. IO 타입
### 3.1 개념
- 프로그램이 외부와 상호작용하기 위한 특별한 타입
- 입력과 출력 같은 실제 세계와의 상호작용을 안전하게 처리

### 3.2 주요 IO 함수들
```haskell
putStr   :: String -> IO ()        -- 문자열 출력
putStrLn :: String -> IO ()        -- 문자열 출력 + 개행
getLine  :: IO String              -- 사용자 입력 받기
readFile :: String -> IO String    -- 파일 읽기
writeFile:: String -> String -> IO () -- 파일 쓰기
```

## 4. 실제 사용 예시
### 4.1 간단한 입출력
```haskell
main :: IO ()
main = do
    putStrLn "당신의 이름은?"     -- 출력
    name <- getLine               -- 입력
    putStrLn ("안녕하세요 " ++ name ++ "님!")
```

### 4.2 파일 처리
```haskell
main :: IO ()
main = do
    -- 파일 쓰기
    writeFile "hello.txt" "안녕하세요!"
    
    -- 파일 읽기
    content <- readFile "hello.txt"
    putStrLn content
```

## 5. do 표기법
- 여러 IO 작업을 순차적으로 실행할 때 사용
- 읽기 쉽고 이해하기 쉬운 코드 작성 가능
```haskell
main :: IO ()
main = do
    putStrLn "첫 번째 작업"
    putStrLn "두 번째 작업"
    name <- getLine           -- <- 를 사용해 IO 작업의 결과를 변수에 저장
    putStrLn ("입력값: " ++ name)
```

## 6. 다른 언어와의 비교

### 6.1 Python
```python
# Python
def main():
    name = input("당신의 이름은?")
    print(f"안녕하세요 {name}님!")
```

### 6.2 Java
```java
// Java
public static void main(String[] args) {
    Scanner scanner = new Scanner(System.in);
    System.out.println("당신의 이름은?");
    String name = scanner.nextLine();
    System.out.println("안녕하세요 " + name + "님!");
}
```

### 6.3 Haskell
```haskell
-- Haskell
main :: IO ()
main = do
    putStrLn "당신의 이름은?"
    name <- getLine
    putStrLn ("안녕하세요 " ++ name ++ "님!")
```


## 7. IO 타입이 필요한 이유

### 7.1 순수 함수의 특징과 한계
```haskell
-- 순수 함수의 예
add :: Int -> Int -> Int
add x y = x + y  -- 2와 3을 입력하면 항상 5가 나옴
```
- 순수 함수는 같은 입력에 대해 항상 같은 출력을 반환
- 외부 상태에 의존하지 않음
- 부작용(side effect)이 없음

### 7.2 현실 세계의 필요성
```haskell
-- 이런 함수들은 매번 다른 결과가 나올 수 있음
getCurrentTime    -- 현재 시간
readFile "a.txt"  -- 파일 내용
getLine           -- 사용자 입력
```
- 프로그램은 결국 현실 세계와 상호작용해야 함
- 파일 읽기/쓰기, 네트워크 통신, 사용자 입력 등이 필요
- 이러한 작업들은 순수 함수의 원칙을 위반

### 7.3 IO 타입의 해결책
```haskell
-- IO 타입 사용
readFile :: String -> IO String
getLine :: IO String

main :: IO ()
main = do
    content <- readFile "input.txt"  -- 외부 작업을 IO 타입으로 감싸서 안전하게 처리
    putStrLn content
```

1. **순수성과 현실의 타협**
   - IO 타입은 "이 함수는 외부 세계와 상호작용할 수 있다"고 명시적으로 선언
   - 컴파일러가 순수 함수와 비순수 함수를 구분할 수 있게 됨

2. **타입 시스템을 통한 안전성**
   ```haskell
   pureFunction :: String -> String         -- 순수 함수
   impureFunction :: String -> IO String    -- 비순수 함수
   ```
   - 어떤 함수가 부작용을 가질 수 있는지 타입만 보고도 알 수 있음
   - 실수로 순수 함수와 비순수 함수를 섞어 쓰는 것을 방지

3. **제어된 부작용**
   ```haskell
   main :: IO ()
   main = do
       let x = 5                  -- 순수한 계산
       putStrLn (show x)         -- IO 작업 (부작용)
       content <- readFile "a.txt" -- IO 작업 (부작용)
   ```
   - 부작용이 있는 코드를 명확히 구분
   - 부작용을 가진 코드의 실행 순서를 제어

4. **코드의 예측 가능성**
   ```haskell
   -- 이 함수의 결과는 외부 상태에 따라 달라질 수 있다는 것을 타입으로 명시
   readUserConfig :: IO Configuration
   
   -- 이 함수는 순수하며 항상 같은 결과를 반환
   processConfig :: Configuration -> Results
   ```
   - 어떤 코드가 예측 불가능한 결과를 만들 수 있는지 명확히 알 수 있음
   - 버그 발생 가능성이 있는 부분을 쉽게 식별

### 7.4 실제 예시로 보는 IO 타입의 필요성
```haskell
-- 잘못된 접근 (순수 함수로 처리하려고 할 때)
getCurrentUserName :: String          -- 이렇게 하면 안 됨!
getCurrentTime :: Time               -- 이렇게 하면 안 됨!

-- 올바른 접근 (IO 타입 사용)
getCurrentUserName :: IO String       -- 올바른 방법
getCurrentTime :: IO Time            -- 올바른 방법

main :: IO ()
main = do
    name <- getCurrentUserName
    time <- getCurrentTime
    putStrLn $ "User " ++ name ++ " logged in at " ++ show time
```

이처럼 IO 타입은 Haskell에서 순수성과 현실 세계의 요구사항을 조화롭게 만드는 핵심 메커니즘입니다. 프로그램의 순수한 부분과 부작용이 있는 부분을 명확히 구분하여, 안전하고 예측 가능한 코드를 작성할 수 있게 해줍니다.
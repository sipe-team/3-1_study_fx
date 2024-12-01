# Haskell Text Processing Guide

## 1. String Basics

### String Type

- `String`은 `[Char]`의 type synonym (동의어)입니다
- `type String = [Char]`로 정의되어 있음
- 실제로는 문자들의 리스트를 다루는 것과 동일

```haskell
ghci> :t "Hello"
"Hello" :: String
ghci> :t ['H','e','l','l','o']
['H','e','l','l','o'] :: [Char]
```

### String Literal

- 큰따옴표로 문자열 표현: `"Hello"`
- 문자 리스트로 표현: `['H','e','l','l','o']`
- 두 표현은 완전히 동일
- 'h' : 'e' : 'l' :'l' : 'o' : [] == "hello"

## 2. 문자열 조작

### 기본 연산

```haskell
-- 문자열 연결
"Hello" ++ " World"  -- "Hello World"

-- 문자열 분해
head "Hello"         -- 'H' 첫 char 만 추출
last "Hello"         -- 'o' 마지막 char 추출

tail "Hello"         -- "ello" 첫 char 제외한 나머지 리스트 추출
init "Hello"         -- "Hell" 마지막 char 제외한 나머지 추출

-- 문자열 검사
null ""             -- True
null "Hello"        -- False
length "Hello"      -- 5
```

### 문자열 처리 함수

-- 문자열 뒤집기

```haskell
reverse :: [a] -> [a] -- 리스트의 순서를 뒤집는 함수
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

-- 예제
reverse "hello" -- "olleh"
reverse [1,2,3,4,5] -- [5,4,3,2,1]
```

```haskell
-- 대소문자 변환
import Data.Char
map toUpper "hello"    -- "HELLO"
map toLower "HELLO"    -- "hello"
```

```haskell
-- 문자열 필터링
filter isA lpha "Hello123"  -- "Hello"
filter isDigit "Hello123"  -- "123"
```

```haskell
-- 문자열 검색
elem :: Eq a => a -> [a] -> Bool -- 리스트에 특정 요소가 있는지 확인하는 함수
elem x [] = False
elem x (y:ys) = x == y || elem x ys

-- 예제
elem 'e' "hello" -- True
elem 'x' "hello" -- False
elem 3 [1,2,3,4,5] -- True
```






## 3. 성능 고려사항

### String의 한계

- `String`은 linked list로 구현되어 있어 큰 텍스트 처리에 비효율적
- 문자열 연결 작업이 O(n) 시간 복잡도
- 메모리 사용량이 높음

### 대안

- `Text` (from `Data.Text`): UTF-16 인코딩 사용, 효율적인 텍스트 처리
- `ByteString` (from `Data.ByteString`): 바이트 단위 처리, 바이너리 데이터에 적합

```haskell
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Text 사용 예시
text1 = T.pack "Hello"    -- String을 Text로 변환
text2 = T.unpack text1    -- Text를 String으로 변환
```

## 4. 문자열 패턴 매칭

```haskell
-- 패턴 매칭 예시
processString :: String -> String
processString [] = "Empty string"
processString (x:xs) = "Starts with " ++ [x]

-- 가드를 이용한 패턴 매칭
classifyString :: String -> String
classifyString str
  | null str = "Empty"
  | length str < 5 = "Short"
  | otherwise = "Long"
```

## 5. 실용적인 문자열 처리

### 입출력

```haskell
-- 문자열 입출력
main :: IO ()
main = do
  putStrLn "Enter your name:"
  name <- getLine
  putStrLn $ "Hello, " ++ name

-- 파일 읽기
readFile "input.txt"  -- IO String 반환
```

### 문자열 파싱

```haskell
-- words와 lines 사용
words "Hello World"   -- ["Hello","World"]
lines "Hello\nWorld"  -- ["Hello","World"]

-- 문자열을 숫자로 변환
read "42" :: Int      -- 42
show 42               -- "42"
```

## 6. 좋은 실천 방법

- 큰 텍스트 처리할 때는 `Text` 타입 사용
- 문자열 연결이 많은 경우 `concat` 또는 `intercalate` 사용
- 유니코드 처리가 필요한 경우 `Text` 사용 권장
- 성능이 중요한 경우 `String` 대신 `Text` 또는 `ByteString` 사용

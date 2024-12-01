# Haskell의 Prelude 모듈 완벽 가이드

## 1. Prelude란?
Prelude는 Haskell의 표준 기본 모듈입니다. 모든 Haskell 프로그램에 자동으로 import되며, 가장 기본적이고 자주 사용되는 함수들과 타입들을 포함합니다.

```haskell
-- Prelude는 자동으로 import 되므로 일반적으로 명시할 필요가 없음
import Prelude  -- 이렇게 명시적으로 import할 필요 없음
```

## 2. Prelude의 주요 구성 요소

### 2.1 기본 타입들
```haskell
-- 기본 데이터 타입
Bool    -- True, False
Char    -- 문자
String  -- 문자열 ([Char]의 타입 별칭)
Int     -- 정수
Integer -- 임의 정밀도 정수
Float   -- 단정밀도 부동소수점
Double  -- 배정밀도 부동소수점
```

### 2.2 주요 함수들

#### 리스트 관련 함수
```haskell
head :: [a] -> a                -- 리스트의 첫 번째 요소
tail :: [a] -> [a]              -- 첫 번째 요소를 제외한 나머지
length :: [a] -> Int            -- 리스트의 길이
reverse :: [a] -> [a]           -- 리스트 뒤집기
take :: Int -> [a] -> [a]       -- n개의 요소 가져오기
drop :: Int -> [a] -> [a]       -- n개의 요소 제거하기
(++) :: [a] -> [a] -> [a]       -- 리스트 연결
```

#### 수학 함수
```haskell
(+) :: Num a => a -> a -> a     -- 더하기
(-) :: Num a => a -> a -> a     -- 빼기
(*) :: Num a => a -> a -> a     -- 곱하기
(/) :: Fractional a => a -> a -> a  -- 나누기
max :: Ord a => a -> a -> a     -- 최대값
min :: Ord a => a -> a -> a     -- 최소값
```

#### 입출력 함수
```haskell
putStr :: String -> IO ()       -- 문자열 출력
putStrLn :: String -> IO ()     -- 문자열 출력 + 개행
print :: Show a => a -> IO ()   -- 값 출력
getLine :: IO String            -- 한 줄 입력 받기
```

### 2.3 주요 타입 클래스
```haskell
class Eq a where                -- 동등성 비교
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

class Ord a where               -- 순서 비교
    compare :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool

class Show a where              -- 문자열로 변환
    show :: a -> String

class Read a where              -- 문자열에서 값으로 변환
    read :: String -> a
```

## 3. Prelude 사용 예시

### 3.1 기본 연산
```haskell
main :: IO ()
main = do
    -- 리스트 조작
    let numbers = [1,2,3,4,5]
    print (head numbers)        -- 1
    print (tail numbers)        -- [2,3,4,5]
    print (reverse numbers)     -- [5,4,3,2,1]
    
    -- 수학 연산
    print (max 3 4)            -- 4
    print (min 3 4)            -- 3
    
    -- 타입 변환
    print (show 123)           -- "123"
    print (read "123" :: Int)  -- 123
```

### 3.2 Prelude 함수 숨기기
```haskell
-- head 함수를 직접 구현하고 싶을 때
import Prelude hiding (head)

head :: [a] -> a
head (x:_) = x
head [] = error "Empty list"
```

## 4. Prelude 관련 팁과 주의사항

### 4.1 이름 충돌 방지
```haskell
-- 방법 1: hiding 사용
import Prelude hiding (length)

-- 방법 2: qualified import 사용
import qualified Prelude as P
```

### 4.2 자주 하는 실수
```haskell
-- 잘못된 예
head []  -- 런타임 에러!

-- 안전한 버전 사용
import Data.Maybe (listToMaybe)
listToMaybe :: [a] -> Maybe a  -- 빈 리스트에 대해 Nothing 반환
```

### 4.3 성능 고려사항
```haskell
-- 비효율적인 사용
reverse [1..1000000]  -- 메모리를 많이 사용

-- 효율적인 대안
import Data.List (foldl')
myReverse = foldl' (flip (:)) []
```

## 5. NoImplicitPrelude 확장

GHC는 `NoImplicitPrelude` 확장을 제공하여 Prelude의 자동 import를 비활성화할 수 있습니다:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

-- 이제 필요한 것만 명시적으로 import
import Prelude (Int, String, IO, putStrLn)
```

## 6. 대체 Prelude들

Haskell 커뮤니티에서는 여러 대체 Prelude들이 개발되었습니다:

1. **base-prelude**: 최소화된 Prelude
2. **protolude**: 현대적이고 안전한 Prelude
3. **classy-prelude**: 타입클래스 중심의 Prelude

```haskell
-- protolude 사용 예
{-# LANGUAGE NoImplicitPrelude #-}
import Protolude

main :: IO ()
main = putStrLn "Hello, World!"
```

## 정리
Prelude는 Haskell 프로그래밍의 기초를 제공하는 핵심 모듈입니다. 기본 타입, 함수, 타입클래스들을 포함하며, 필요에 따라 숨기거나 수정할 수 있습니다. Prelude의 기능들을 잘 이해하고 활용하는 것이 Haskell 프로그래밍의 기본입니다.
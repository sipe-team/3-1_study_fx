# Haskell 모듈 시스템 가이드

## 목차
- [Haskell 모듈 시스템 가이드](#haskell-모듈-시스템-가이드)
  - [목차](#목차)
  - [모듈 기본 구조](#모듈-기본-구조)
  - [모듈 가져오기 (Import)](#모듈-가져오기-import)
  - [모듈 내보내기 (Export)](#모듈-내보내기-export)
    - [기본 내보내기 구문](#기본-내보내기-구문)
    - [데이터 타입 정의 예시](#데이터-타입-정의-예시)
    - [내보내기 옵션](#내보내기-옵션)
  - [모듈 계층 구조](#모듈-계층-구조)
  - [실제 사용 예시](#실제-사용-예시)
    - [Stack 구현 예제](#stack-구현-예제)
  - [모듈 시스템의 장점](#모듈-시스템의-장점)
  - [주의사항](#주의사항)

## 모듈 기본 구조

모듈은 다음과 같은 기본 구조를 가집니다:

```haskell
module ModuleName 
    ( export1
    , export2
    , export3
    ) where

-- 모듈 내용
```

## 모듈 가져오기 (Import)

다양한 방식으로 모듈을 가져올 수 있습니다:

```haskell
-- 전체 모듈 가져오기
import Data.List

-- 특정 함수만 가져오기
import Data.List (sort, nub)

-- qualified import (이름 충돌 방지)
import qualified Data.Map as Map

-- hiding (특정 함수 제외하고 가져오기)
import Data.List hiding (sort)
```

## 모듈 내보내기 (Export)

### 기본 내보내기 구문

```haskell
-- 파일명과 module 명이 동일해야함. (대소문자 주의)
module MyModule 
    ( -- 타입 내보내기
      Person(..)    -- 데이터 생성자도 모두 내보냄
    , Animal(Dog)   -- Dog 생성자만 내보냄
    
    -- 함수 내보내기
    , calculateAge
    , getName
    
    -- 타입 클래스 내보내기
    , MyClass(..)   -- 클래스의 모든 메서드 내보냄
    ) where
```

### 데이터 타입 정의 예시

```haskell
data Person = Person String Int
data Animal = Dog String | Cat String

calculateAge :: Person -> Int
calculateAge (Person _ age) = age

getName :: Person -> String
getName (Person name _) = name

class MyClass a where
    myMethod :: a -> String
```

### 내보내기 옵션
* `Type(..)`: 모든 생성자를 함께 내보냅니다.
* `Type(Constructor1, Constructor2)`: 특정 생성자만 내보냅니다.
* `Type`: 타입만 내보내고 생성자는 감춥니다 (추상 데이터 타입).

## 모듈 계층 구조

모듈은 계층적으로 구성할 수 있습니다:

```haskell
-- 하위 모듈 정의
module Data.MyModule.SubModule where

-- 하위 모듈 가져오기
import Data.MyModule.SubModule
```

## 실제 사용 예시

### Stack 구현 예제

```haskell
-- MyLib.hs
module MyLib 
    ( Stack
    , empty
    , push
    , pop
    ) where

data Stack a = Stack [a]

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack []) = Nothing
pop (Stack (x:xs)) = Just (x, Stack xs)

-- Main.hs
import MyLib

main :: IO ()
main = do
    let s1 = push 1 empty
    let s2 = push 2 s1
    print $ pop s2
```

## 모듈 시스템의 장점

1. **캡슐화**
   - 내부 구현을 숨기고 안정적인 인터페이스만 제공
   - 구현 세부사항 변경 시 외부 코드에 영향 최소화

2. **이름 충돌 방지**
   - qualified import를 통한 이름 공간 관리
   - 서로 다른 모듈의 동일한 이름 함수 사용 가능

3. **코드 구조화**
   - 관련 기능을 모듈 단위로 구조화
   - 코드의 논리적 구성과 관리 용이

4. **재사용성**
   - 잘 설계된 모듈은 다른 프로젝트에서도 재사용 가능
   - 모듈 단위의 테스트와 유지보수 용이

## 주의사항

1. 모듈 이름은 대문자로 시작해야 합니다.
2. 파일 이름은 모듈 이름과 일치해야 합니다 (예: `MyModule.hs`).
3. 내보내지 않은 함수나 타입은 모듈 외부에서 접근할 수 없습니다.
4. 순환 의존성을 피해야 합니다.
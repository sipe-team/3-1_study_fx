# Haskell 데이터 타입 정의 가이드

## 1. 데이터 타입 정의 기본 문법

```haskell
data TypeName = Constructor1 | Constructor2 | ... | ConstructorN
```

- **data**: 새로운 데이터 타입을 정의하는 키워드
- **TypeName**: 타입의 이름 (대문자로 시작)
- **Constructor**: 값 생성자 (대문자로 시작)
- **|**: "또는(or)"을 의미

## 2. deriving 절

```haskell
data TypeName = Constructor1 | Constructor2 deriving (TypeClass1, TypeClass2)
```

주요 타입클래스:

- **Show**: 값을 문자열로 표현
- **Eq**: 값들의 동등성 비교
- **Ord**: 값들의 순서 비교
- **Enum**: 순차적인 값들
- **Read**: 문자열에서 값으로 변환
- **Bounded**: 최소/최대 값 정의

## 3. 데이터 타입 예시

### 3.1 단순 열거형

```haskell
-- 카드의 무늬
data Suit = Hearts | Diamonds | Clubs | Spades
    deriving (Show, Eq)

-- 요일
data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Show, Eq, Enum)

-- 신호등
data TrafficLight = Red | Yellow | Green
    deriving (Show, Eq)
```

### 3.2 값을 가지는 생성자

```haskell
-- 좌표점
data Point = Point Double Double
    deriving (Show, Eq)

-- 도형
data Shape = Circle Point Double    -- 중심점과 반지름
          | Rectangle Point Point   -- 좌상단점과 우하단점
          | Triangle Point Point Point  -- 세 꼭지점
    deriving (Show, Eq)
```

### 3.3 레코드 구문

```haskell
-- 사용자 정보
data User = User
    { userName :: String
    , userAge :: Int
    , userEmail :: String
    }
    deriving (Show, Eq)

-- 학생 정보
data Student = Student
    { studentId :: String
    , studentName :: String
    , studentGrade :: Int
    , studentMajor :: String
    }
    deriving (Show, Eq)
```

### 3.4 타입 매개변수

```haskell
-- 컨테이너 타입
data Box a = Empty | Box a
    deriving (Show, Eq)

-- 이진 트리
data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Show, Eq)

-- 결과 타입
data Result a = Success a | Error String
    deriving (Show, Eq)
```

## 4. 사용 예시

### 4.1 기본 사용

```haskell
-- Suit 타입 사용
let myCard = Hearts

-- 출력 (Show)
ghci> print myCard
Hearts

-- 비교 (Eq)
ghci> Hearts == Spades
False
```

### 4.2 패턴 매칭

```haskell
-- Suit에 대한 패턴 매칭
getSuitColor :: Suit -> String
getSuitColor Hearts = "Red"
getSuitColor Diamonds = "Red"
getSuitColor Clubs = "Black"
getSuitColor Spades = "Black"

-- Shape에 대한 패턴 매칭
area :: Shape -> Double
area (Circle _ r) = pi * r * r
area (Rectangle (Point x1 y1) (Point x2 y2)) = abs ((x2 - x1) * (y2 - y1))
area (Triangle _ _ _) = undefined  -- 구현 필요
```

### 4.3 레코드 사용

```haskell
-- 생성
let user1 = User
    { userName = "John"
    , userAge = 30
    , userEmail = "john@example.com"
    }

-- 필드 접근
ghci> userName user1
"John"

-- 레코드 업데이트
let user2 = user1 { userAge = 31 }
```

## 5. 주의사항과 팁

1. **이름 규칙**

   - 타입 이름과 생성자는 대문자로 시작
   - 함수와 변수는 소문자로 시작

2. **타입클래스 선택**

   - 필요한 기능에 따라 적절한 타입클래스 선택
   - 너무 많은 타입클래스는 피하기

3. **레코드 사용**
   - 여러 필드가 있는 경우 레코드 구문 사용
   - 필드 이름 중복 주의

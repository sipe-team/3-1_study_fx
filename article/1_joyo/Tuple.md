# Haskell Tuple Types

## 1. 튜플(Tuple) 개요

튜플은 서로 다른 타입의 값들을 하나의 값으로 묶을 수 있는 자료구조입니다. 튜플은 다음과 같은 특징을 가집니다:

- 고정된 크기를 가짐
- 서로 다른 타입의 값을 포함할 수 있음
- 괄호와 쉼표로 표현: `(값1, 값2, ...)`
- 가장 많이 사용되는 것은 2개의 값을 가진 페어(Pair)와 3개의 값을 가진 트리플(Triple)

## 2. 기본 문법

```haskell
-- 타입 선언
type Point = (Int, Int)
type Person = (String, Int)
type BookInfo = (String, String, Int)  -- (제목, 저자, 페이지수)

-- 값 생성
point :: Point
point = (10, 20)

person :: Person
person = ("John", 25)

book :: BookInfo
book = ("Haskell Programming", "Author Name", 300)
```

## 3. 예시

### 예시 1: 좌표 시스템
```haskell
-- 2D 점과 거리 계산
type Point2D = (Double, Double)

distance :: Point2D -> Point2D -> Double
distance (x1, y1) (x2, y2) = sqrt (dx * dx + dy * dy)
    where
        dx = x2 - x1
        dy = y2 - y1

-- 사용 예시
point1 :: Point2D
point1 = (0.0, 0.0)

point2 :: Point2D
point2 = (3.0, 4.0)

main1 :: IO ()
main1 = do
    let dist = distance point1 point2
    putStrLn $ "Distance between points: " ++ show dist  -- 결과: 5.0
```

### 예시 2: 학생 성적 관리
```haskell
-- 학생 정보와 성적 처리
type Student = (String, Int, [Int])  -- (이름, 학번, 점수리스트)

calculateAverage :: Student -> (String, Double)
calculateAverage (name, _, scores) = (name, average)
    where
        average = fromIntegral (sum scores) / fromIntegral (length scores)

processStudents :: [Student] -> [(String, Double)]
processStudents = map calculateAverage

-- 사용 예시
students :: [Student]
students = [
    ("Alice", 1001, [90, 85, 95]),
    ("Bob", 1002, [75, 80, 85]),
    ("Charlie", 1003, [95, 90, 100])
    ]

main2 :: IO ()
main2 = do
    let results = processStudents students
    mapM_ (\(name, avg) -> 
        putStrLn $ name ++ "'s average: " ++ show avg) results
```

### 예시 3: 도서관 도서 관리
```haskell
-- 도서 정보와 대출 상태 관리
type Book = (String, String, Int)  -- (제목, 저자, 출판연도)
type LibraryItem = (Book, Bool, Maybe String)  -- (책정보, 대출가능여부, 대출자)

checkoutBook :: LibraryItem -> String -> LibraryItem
checkoutBook ((title, author, year), True, Nothing) borrower = 
    ((title, author, year), False, Just borrower)
checkoutBook book _ = book

returnBook :: LibraryItem -> LibraryItem
returnBook ((title, author, year), _, _) = 
    ((title, author, year), True, Nothing)

displayBookInfo :: LibraryItem -> String
displayBookInfo ((title, author, year), available, borrower) =
    title ++ " by " ++ author ++ " (" ++ show year ++ ")\n" ++
    "Status: " ++ (if available then "Available" else "Checked out") ++
    case borrower of
        Just name -> " to " ++ name
        Nothing   -> ""

-- 사용 예시
library :: [LibraryItem]
library = [
    (("The Hobbit", "J.R.R. Tolkien", 1937), True, Nothing),
    (("1984", "George Orwell", 1949), False, Just "Alice"),
    (("Dune", "Frank Herbert", 1965), True, Nothing)
    ]

main3 :: IO ()
main3 = do
    -- 도서 정보 출력
    mapM_ (putStrLn . displayBookInfo) library
    -- 도서 대출
    let updatedLibrary = checkoutBook (head library) "Bob"
    putStrLn "\nAfter checkout:"
    putStrLn $ displayBookInfo updatedLibrary
```

## 4. 유용한 튜플 함수

```haskell
-- 기본 제공 함수들
fst :: (a, b) -> a            -- 첫 번째 요소 추출
snd :: (a, b) -> b            -- 두 번째 요소 추출
swap :: (a, b) -> (b, a)      -- 요소의 순서 바꾸기
curry :: ((a, b) -> c) -> a -> b -> c    -- 튜플 함수를 커리된 함수로 변환
uncurry :: (a -> b -> c) -> (a, b) -> c  -- 커리된 함수를 튜플 함수로 변환
```

튜플은 간단한 데이터 구조가 필요할 때 매우 유용하지만, 더 복잡한 데이터 구조가 필요한 경우에는 사용자 정의 데이터 타입(`data`)를 사용하는 것이 더 적절할 수 있습니다.
## 하스켈의 함수

### 하스켈의 오직 하나의 parameter 만 받을 수 있다.

- 여러가지 예시들

  ```haskell
  double :: Int -> Int
  double n = n * 2

  perimeter :: Int -> Int -> Int
  perimeter w h = double (w + h)

  xOr :: Bool -> Bool -> Bool
  xOr a b = (a || b) && not (a && b)

  factorial :: Integer -> Integer
  factorial n = if n == 0 then 1 else n * factorial (n - 1)

  nand :: Bool -> Bool -> Bool
  nand True True = False
  nand _ _ = True
  ```

- 함수 패턴 매칭 방식으로 정의

  - 패턴 매칭의 주요 장점들:

    - 코드가 더 읽기 쉽고 명확해집니다
    - 컴파일러가 모든 경우를 처리했는지 확인할 수 있습니다
    - 데이터 구조를 쉽게 분해할 수 있습니다

  - 패턴 매칭을 사용할 때의 중요한 규칙:
    - 위에서 아래로 순서대로 패턴을 검사합니다
    - 더 구체적인 패턴을 위에 작성해야 합니다
    - 모든 경우를 처리해야 합니다

  ```haskell
  -- 패턴 매칭을 사용한 기본 예시
  factorial :: Integer -> Integer
  factorial 0 = 1
  factorial n = n * factorial (n - 1)

  -- 리스트 패턴 매칭
  length' :: [a] -> Int
  length' [] = 0              -- 빈 리스트 패턴
  length' (_:xs) = 1 + length' xs  -- head와 tail로 분해하는 패턴, _ 는 anonymous variable 의미

  -- 튜플 패턴 매칭
  fst' :: (a, b) -> a
  fst' (x, _) = x    -- 첫 번째 요소만 추출

  -- 가드를 함께 사용한 패턴 매칭
  absolute :: Int -> Int
  absolute n
    | n >= 0    = n
    | otherwise = -n

  -- 복잡한 데이터 타입의 패턴 매칭
  data Shape = Circle Float | Rectangle Float Float
  area :: Shape -> Float
  area (Circle r) = pi * r * r
  area (Rectangle w h) = w * h
  ```

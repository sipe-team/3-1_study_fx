## 하스켈의 함수

- 예시

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

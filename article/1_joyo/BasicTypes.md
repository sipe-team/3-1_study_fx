## 하스켈의 Basic Types

- Booleans: Bool
- Integers: Int, Integer
- Floating-point numbers: Float, Double
- Characters: Char
- Strings: String
- Lists: [a]
- Tuples: (a, b, c)

## 기억하면 좋을 용어 멘탈모델

- :: "타입은..." (타입 시그니처)
- = "정의한다" (함수 정의)
- let "변수를 선언한다" (변수 선언)
- in "그리고 사용한다" (함수 사용)
- where "조건을 추가한다" (조건 추가)
- => "~라는 조건을 만족한다면" (타입 제약 조건)
- -> "~를 받아서 ~를 반환한다" (함수 타입 표현)

## 코드 읽기 예시들

```haskell
-- -> 만 사용
length :: [a] -> Int
-- "어떤 타입 a의 리스트를 받아서 Int를 반환한다"

-- 둘 다 사용
sort :: Ord a => [a] -> [a]
-- "a가 Ord의 인스턴스라면,
--  a의 리스트를 받아서 a의 리스트를 반환한다"

show :: Show a => a -> String
-- "a가 Show의 인스턴스라면,
--  a를 받아서 String을 반환한다"
```

```haskell
min :: Ord a => a -> a -> a
--     ^^^^^^    ^    ^    ^
--     (조건)    |    |    |
--           입력1 입력2  출력

compare :: Ord a => a -> a -> Ordering
--        ^^^^^^    ^    ^    ^^^^^^^^
--        (조건)    |    |    (반환 타입)
--              입력1 입력2
```

## 몇 가지 주의사항

1. 음수 표현 - 하스켈에서는 음수를 표현할 때 (괄호) 를 사용해야 함.
2. 다르다 비교 연산자를 != 가 아니라 /= 이다.
3. mod 와 rem 함수는 나머지를 계산하지만 음수 처리 방식이 다름. rem (-11) 2 = -1, mod (-11) 2 = 1
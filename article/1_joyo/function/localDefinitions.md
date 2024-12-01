# Haskell 지역 정의: `let-in`과 `where`

Haskell은 지역 정의를 위해 두 가지 주요 문법을 제공합니다: `let-in`과 `where`. 두 방식 모두 비슷한 목적으로 사용되지만, 각각의 특징과 용도가 다릅니다.

## 1. `let-in` 문법

```haskell
let 정의부 in 표현식
```

`let-in`은 표현식 내에서 사용할 수 있는 지역 바인딩을 생성합니다. `let`은 그 자체로 표현식이므로 표현식이 허용되는 모든 곳에서 사용할 수 있습니다.

### 예시:

```haskell
calculateArea :: Double -> Double -> Double
calculateArea width height =
    let area = width * height
        perimeter = 2 * (width + height)
    in  area + perimeter

-- 어떤 표현식 문맥에서도 사용 가능
factorial :: Integer -> Integer
factorial n =
    let helper acc 0 = acc
        helper acc n = helper (acc * n) (n - 1)
    in  helper 1 n
```

### 주요 특징:

- 정의가 사용보다 먼저 나옴
- 표현식 중심
- 모든 표현식 문맥에서 사용 가능
- 들여쓰기를 통한 여러 정의 가능
- 정의에서 패턴 매칭 가능

## 2. `where` 문법

```haskell
표현식 where 정의부
```

`where`는 함수 정의의 끝에서 지역 바인딩을 생성합니다. 주요 계산을 먼저 작성하고 그 구성 요소를 나중에 정의할 수 있습니다.

### 예시:

```haskell
calculateArea :: Double -> Double -> Double
calculateArea width height = area + perimeter
    where
        area = width * height
        perimeter = 2 * (width + height)

-- 패턴 매칭을 활용한 복잡한 예시
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = smaller ++ [x] ++ larger
    where
        smaller = quickSort [a | a <- xs, a <= x]
        larger  = quickSort [a | a <- xs, a > x]
```

### 주요 특징:

- 정의가 사용 후에 나옴
- 선언 중심
- 전체 함수 정의에 대한 스코프
- 들여쓰기를 통한 여러 정의 가능
- 정의에서 패턴 매칭 가능

## 3. 주요 차이점

### 스코프

- `let-in`: `in` 뒤의 표현식에만 한정
- `where`: 전체 함수 정의나 패턴 매칭에 적용

### 순서

- `let-in`: 사용하기 전에 정의 (위에서 아래로 읽기)
- `where`: 사용한 후에 정의 (아래에서 위로 읽기)

### 사용 맥락

- `let-in`: 모든 표현식 문맥에서 사용 가능
- `where`: 함수 정의나 패턴 매칭의 끝에서만 사용 가능

## 4. 사용 가이드라인

### let-in을 사용하는 경우:

- 표현식 내의 임시 바인딩
- 정의가 간단하고 사용과 밀접하게 관련된 경우
- do 표기법이나 리스트 해석에서
- 바인딩을 표현식으로 필요로 하는 경우

### where를 사용하는 경우:

- 복잡한 함수를 분해할 때
- 여러 가드나 패턴에서 정의를 공유할 때
- 주요 계산을 강조하고 싶을 때
- 여러 방정식에서 정의를 공유할 때

## 5. 함께 사용하는 예시

```haskell
complexCalculation :: Double -> Double -> Double
complexCalculation x y =
    let intermediate = preliminary * factor
    in  final + intermediate
    where
        preliminary = x * y
        factor = 2.0
        final = preliminary + y
```

이 예시는 두 방식을 함께 사용할 수 있음을 보여주지만, 코드의 명확성을 위해 이러한 방식은 신중하게 사용해야 합니다.

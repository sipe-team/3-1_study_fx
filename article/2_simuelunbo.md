# 함수형 프로그래밍이란

위키피디아에서 함수형 프로그래밍 정의:

- 수학적 함수를 사용하고 부작용, 즉 부수 효과(side effect)를 피하는 것이 특징인 프로그래밍 패러다임
- 부수 효과 없이 순수 함수(pure function)만 사용하는 프로그래밍 스타일

## 부수 효과

부수 효과(side effect)는 프로그래밍에서 함수나 메소드가 무언가를 할 때 그 주된 목적 외에 추가적인 일이 일어나는 것을 의미 
즉 같은 입력을 넣더라도 반환값이 동일하지 않을 수 있다.

### Example

```haskell
import System.Random

rollDice :: IO Int
rollDice = do
    number <- randomRIO (1, 6)  -- 매번 다른 결과가 나올 수 있음(부수 효과)
    return number

main :: IO ()
main = do
    dice1 <- rollDice
    dice2 <- rollDice
    -- 같은 코드를 실행해도 dice1과 dice2는 매번 다른 값이 나올 수 있다
    putStrLn $ "결과: " ++ show (dice1, dice2)
```

위의 예제에서 `rollDice` 함수는 매번 다른 결과를 반환 
주사위를 굴리는 행위는 외부의 랜덤 상태에 의존하기 때문에 **부수 효과**가 있기 때문

## 순수 함수

순수 함수는 인자에만 의존하여 동일한 인자를 넣었을 때 항상 같은 결과를 돌려주는 함수 
이런 함수는 예측 가능하고 테스트하기 쉬워짐

### Example

```haskell
add :: Int -> Int -> Int
add x y = x + y

main :: IO ()
main = do
    let result = add 3 4
    putStrLn ("The result is: " ++ show result)
```

위의 `add` 함수는 주어진 입력값에 대해 항상 같은 출력을 반환 같은 인자를 사용하면 동일한 결과를 얻기 때문에 이 함수는 **부수 효과 X**.

## 실용적인 측면에서 함수형 프로그래밍 정의 문제점

프로그래밍을 하다 보면 실용적인 측면에서 반드시 부수 효과는 필요 
그래서 함수형 프로그래밍에서는 순수하지 않은 함수도 잘 다룰 줄 알아야 함
무조건 부수 효과를 배제하는 것이 아닌 적절한 기술 테크닉을 통해 이를 처리하는 방법을 알아야 함


일반적으로 함수형 프로그래밍을 학술적인 개념으로 많이 이해하고 접근하지만 **학술적 함수형 프로그래밍 != 실용적 함수형 프로그래밍**
이기 때문에 우리는 실용적인 관점에서 기술과 개념으로 접근함 
함수형 프로그래밍의 진정한 가치는 **부수 효과를 통제하고 관리하는 능력**에 있기 때문

## 함수형 프로그래밍의 3가지 핵심 요소: 액션, 계산, 데이터

### 1. 액션 (Actions)

- 실행 시점과 횟수가 중요합니다.
- 같은 입력에도 다른 결과가 나올 수 있습니다.
- 부수 효과를 포함합니다.

```haskell
-- 액션의 예: 랜덤 숫자 생성
generateRandomNumber :: IO Int
generateRandomNumber = randomRIO (1, 100)

-- 액션의 예: 현재 시간 얻기
getCurrentTime :: IO UTCTime
getCurrentTime = Data.Time.getCurrentTime
```

### 2. 계산 (Calculations)

- 같은 입력에 대해 항상 같은 출력을 반환합니다.
- 부수 효과가 없습니다.
- 실행 시점과 횟수가 중요하지 않습니다.
- 참조 투명성을 가집니다.

```haskell
-- 계산의 예: 수학 연산
add :: Int -> Int -> Int
add x y = x + y
```

### 3. 데이터 (Data)

- 정보를 표현하는 불변의 값입니다.
- 시간이 지나도 변하지 않습니다.
- 행위를 가지지 않습니다.

```haskell
-- 데이터의 예
data Person = Person {
    name :: String,
    age :: Int,
    email :: String
}
```


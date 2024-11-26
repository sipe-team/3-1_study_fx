# 모나드의 본질적 이해

## 1. 모나드는 컨텍스트 안의 타입

모나드는 값을 담고 있는 컨텍스트입니다. 중요한 점은

- 다른 타입을 감싸는 타입입니다.
- 이 컨텍스트는 의미를 가집니다.
- 값에 어떤 의미나 효과를 부여합니다.

```haskell
-- Maybe 모나드: 실패할 수 있는 컨텍스트
data Maybe a = Nothing | Just a

-- Either 모나드: 오류 정보를 포함하는 컨텍스트
data Either e a = Left e | Right a
```

## 2. 모나드의 핵심 함수들

모나드는 두 가지 핵심 함수를 가집니다

```haskell
-- 1. return (pure): 값을 모나드로 감싸기
return :: a -> m a

-- 2. bind (>>=): 모나드 값을 다른 모나드 함수에 전달
(>>=) :: m a -> (a -> m b) -> m b
```

예를 들어 은행 계좌 거래를 보면

```haskell
-- 값을 Either 컨텍스트로 감싸기
createAccount :: Amount -> Either Error Account
createAccount amount
    | amount < 0 = Left NegativeAmount
    | otherwise  = Right (Account amount)

-- bind를 사용한 계산 연쇄
deposit :: Account -> Amount -> Either Error Account
deposit account amount =
    if amount < 0
        then Left NegativeAmount
        else Right (account { balance = balance account + amount })
```

## 3. 모나드의 중요한 특징

### 컨텍스트 유지

모나드는 연산을 수행하면서도 컨텍스트를 유지합니다

```haskell
Just 3 >>= (\x -> Just (x + 1))  -- Just 4
Nothing >>= (\x -> Just (x + 1))  -- Nothing
```

### 연쇄적 계산

이전 계산의 결과를 바탕으로 다음 계산을 결정할 수 있습니다

```haskell
transfer :: Account -> Account -> Amount -> Either Error (Account, Account)
transfer from to amount = do
    -- 출금이 성공해야만 입금이 실행됨
    withdrawn <- withdraw from amount    -- 첫 번째 계산
    deposit to amount                    -- 첫 번째 계산 결과에 기반한 두 번째 계산
```

## 4. 모나드의 실제 활용

### Either 모나드로 오류 처리하기

```haskell
data BankError = InsufficientFunds | NegativeAmount

withdraw :: Account -> Amount -> Either BankError Account
withdraw account amount
    | amount < 0 = Left NegativeAmount
    | amount > balance account = Left InsufficientFunds
    | otherwise = Right (account { balance = balance account - amount })
```

이러한 접근의 장점들은 다음과 같습니다.

1. 오류를 명시적으로 처리함.
2. 함수의 시그니처에 실패 가능성이 표현됨.
3. 연쇄적 계산에서 안전성을 보장함.

### Maybe 모나드로 Null 처리하기

```haskell
findUser :: ID -> Maybe User
findAddress :: User -> Maybe Address
findStreet :: Address -> Maybe Street

-- 연쇄적으로 안전하게 street 찾기
getStreet :: ID -> Maybe Street
getStreet id = do
    user <- findUser id
    address <- findAddress user
    findStreet address
```

## 5. 모나드 이해의 핵심 포인트

1. **컨텍스트의 의미**

   - Maybe: 실패할 수 있는 계산
   - Either: 오류 정보를 포함하는 계산
   - List: 여러 가능한 결과를 가진 계산
   - IO: 부작용을 포함하는 계산

2. **바인드(>>=)의 역할**

   - 계산들을 안전하게 연결함.
   - 컨텍스트를 유지하면서 값을 변환함.
   - 중첩된 컨텍스트를 평탄화함.

3. **실용적 이점**

   - 부작용의 명시적 처리
   - 오류 처리의 단순화
   - 안전한 null 처리
   - 비즈니스 로직의 명확한 표현

## 6. 모나드와 어플리커티브의 차이점

모나드와 어플리커티브 펑터는 둘 다 함수의 연산을 적용하기 위한 구조이지만 중요한 차이점이 있습니다.

### 어플리커티브의 특징

어플리커티브는 독립적인 여러 값을 동시에 컨텍스트 안에서 처리할 수 있도록 합니다. 이를 통해 모나드보다 더 간단한 방식으로 연산을 결합할 수 있습니다.

- **pure**: 값을 어플리커티브 컨텍스트로 감싸기 위한 함수입니다.
  ```haskell
  pure :: a -> f a
  ```
- **<\*>** (apply): 컨텍스트 안의 함수와 값을 결합하는 연산자입니다.
  ```haskell
  (<*>) :: f (a -> b) -> f a -> f b
  ```

### 모나드 vs 어플리커티브

- **의존성**: 모나드는 연산이 서로 의존적일 때 사용합니다. 즉, 이전 연산의 결과가 다음 연산의 입력으로 필요할 때입니다. 반면 어플리커티브는 각 연산이 독립적일 때 적합합니다. 여러 연산이 서로 결과에 의존하지 않고 병렬로 결합될 수 있습니다.
- **연산 방식**: 어플리커티브는 모든 값을 독립적으로 평가하여 연산을 적용하지만, 모나드는 평가 결과에 따라 다음 연산을 정의합니다. 따라서 모나드는 좀 더 복잡한 연쇄적 계산을 가능하게 합니다.

### 예시로 이해하기

어플리커티브의 예를 살펴보면:

```haskell
import Control.Applicative

-- 두 Maybe 값을 더하기
add :: Int -> Int -> Int
add x y = x + y

result :: Maybe Int
result = pure add <*> Just 3 <*> Just 5  -- 결과: Just 8
```

위의 코드에서 **pure add**는 함수 `add`를 어플리커티브 컨텍스트로 감싸고, `<*>` 연산자를 사용해 각각의 `Maybe` 값을 결합합니다. 모나드처럼 의존적이지 않고, 독립적으로 값들을 평가할 수 있습니다.

반면, 모나드는 각 계산이 순차적입니다. 이전 계산의 결과가 필요하기 때문에 다음과 같은 코드 구조를 가집니다:

```haskell
resultMonad :: Maybe Int
resultMonad = do
    x <- Just 3
    y <- Just 5
    return (x + y)  -- 결과: Just 8
```

모나드에서는 `x`와 `y`가 순차적으로 평가되고, 그 결과를 이용해 최종 값을 계산합니다.

## 결론

모나드는 단순한 디자인 패턴을 넘어, 복잡한 계산을 안전하고 우아하게 조합할 수 있게 해주는 강력한 추상화입니다. 특히:

- 값을 의미 있는 컨텍스트로 감싸서 안전하게 다룰 수 있게 합니다.
- 복잡한 연산들을 명확하고 안전하게 연결할 수 있게 합니다.
- 부작용과 오류를 명시적이고 우아하게 처리할 수 있게 합니다.

모나드와 어플리커티브의 차이점을 이해하면 언제 어느 것을 사용해야 할지 명확해집니다. 어플리커티브는 독립적인 계산을 쉽게 결합하는 데 적합하고, 모나드는 연쇄적이며 의존적인 계산에 더 적합합니다. 두 가지 모두 함수형 프로그래밍에서 중요한 추상화 도구이며, 이를 활용하면 코드의 안정성과 가독성을 크게 향상시킬 수 있습니다.


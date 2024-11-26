# **하스켈에서의 Applicative**

`Applicative`를 사용하면 `Functor`와 마찬가지로 값이 감싸진 컨텍스트(박스 같은 것) 내에서 작업할 수 있습니다. `Functor`가 값을 감싸는 것뿐만 아니라, `Applicative`를 사용하면 함수도 컨텍스트 안에 감싸서 작업할 수 있습니다.

&#x20;하스켈은 `Applicative`를 내장 지원합니다. 이 추상화는 표준 타입 클래스 계층의 일부로, `Functor`와 `Monad` 사이의 다리를 제공합니다.&#x20;

일반적으로 `Functor`는 `fmap` 함수를 사용하지만, `Applicative`는 `<*>` 연산자(또는 `ap` 함수)를 사용하여 컨텍스트에 감싸진 함수가 컨텍스트에 감싸진 값에 적용되도록 합니다.

## **Applicative 타입 클래스**

`Applicative` 타입 클래스는 다음과 같이 정의됩니다:

```haskell
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

- `pure`는 값을 컨텍스트로 감싸는 데 사용됩니다.
- `<*>`는 컨텍스트에 감싸진 함수를 컨텍스트에 감싸진 값에 적용하는 데 사용됩니다.

## **Maybe 예제**

`Maybe` 타입은 `Applicative`의 인스턴스이기 때문에, `pure`와 `<*>`를 직접 사용하여 `Maybe` 값을 다룰 수 있습니다. 예제를 살펴보겠습니다.

컨텍스트에 감싸진 함수가 다른 `Maybe` 컨텍스트에 있는 값에 적용되기를 원한다고 가정해봅시다:

```haskell
import Control.Applicative

-- 감싸진 함수가 감싸진 값에 적용되는 예제
example1 :: Maybe Int
example1 = Just (+3) <*> Just 2
-- => Just 5
```

이 경우, `Just (+3)`은 `Maybe` 컨텍스트에 감싸진 함수이고, `Just 2`는 우리가 적용하고자 하는 값입니다. `<*>` 연산자는 이 함수를 적용하여 `Just 5`를 반환합니다.

만약 둘 중 하나라도 `Nothing`인 경우 결과는 `Nothing`이 됩니다:

```haskell
example2 :: Maybe Int
example2 = Just (+3) <*> Nothing
-- => Nothing
```

## **리스트 예제**

`Applicative`는 리스트에서도 동작하여, 모든 값의 조합에 대해 함수를 적용할 수 있습니다:

```haskell
example3 :: [Int]
example3 = [(\x -> x * 2), (\x -> x + 3)] <*> [1, 2, 3]
-- => [2, 4, 6, 4, 5, 6]
```

`<*>` 연산자는 리스트의 각 함수가 각 값에 적용되도록 하여 `[2, 4, 6, 4, 5, 6]`을 생성합니다.

## **커링과 Applicative**

`Applicative`가 커링된 함수와 함께 어떻게 동작하는지 살펴봅시다.

두 숫자를 더하는 커링된 함수가 있다고 가정해봅시다:

```haskell
curriedAddition :: Int -> Int -> Int
curriedAddition a b = a + b

example4 :: Maybe Int
example4 = pure curriedAddition <*> Just 3 <*> Just 2
-- => Just 5
```

이 과정을 단계별로 나누어 보겠습니다:

1. `pure curriedAddition`은 `curriedAddition`을 `Maybe` 컨텍스트에 넣습니다: `Maybe (Int -> Int -> Int)`.
2. `<*> Just 3`은 첫 번째 인자를 적용하여 `Maybe (Int -> Int)`를 반환합니다.
3. `<*> Just 2`는 두 번째 인자를 적용하여 `Just 5`를 반환합니다.

이 방법은 인자가 더 많은 함수에도 확장 가능합니다. 예를 들어 세 개의 인자를 받아서 곱하는 함수가 있다고 가정해봅시다:

```haskell
tripleProduct :: Int -> Int -> Int -> Int
tripleProduct a b c = a * b * c

example5 :: Maybe Int
example5 = pure tripleProduct <*> Just 3 <*> Just 5 <*> Just 4
-- => Just 60
```

## **Applicative와 커링**

만약 함수가 커링되어 있지 않다면, 이를 변환하기 위한 헬퍼 함수를 사용할 수 있습니다.\
&#x20;여기서는 세 개의 인자를 가진 함수에 대한 커링 함수를 정의 해봅니다.

```haskell
curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)
```

하지만 하스켈은 기본적으로 커링된 함수와 잘 동작하기 때문에 수동으로 커링할 필요가 없는 경우가 많습니다.

## **요약**

`Applicative` 타입 클래스는 감싸진 값과 함수로 작업할 때 강력한 도구입니다. 하스켈에서는 `pure`를 사용하여 값을 컨텍스트에 넣고 `<*>`를 사용하여 감싸진 함수를 감싸진 값에 적용합니다. 이를 통해 여러 인자를 사용하거나 복잡한 함수도 간단하게 처리할 수 있습니다.

`Applicative`를 사용하면 여러 적용을 깔끔하고 관용적으로 연결할 수 있으며, 수동으로 값을 언랩핑하는 번거로움을 줄이면서도 컨텍스트 내에서 계산을 유지할 수 있습니다.


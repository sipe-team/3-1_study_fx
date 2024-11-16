# Functor에 대하여

## 1. Functor란?

Functor는 함수형 프로그래밍에서 중요한 개념으로, **컨테이너 안에 있는 값을 변환할 수 있게 해주는 구조**라고 이해할 수 있습니다. 예를 들어 리스트나 옵셔널 타입 같은 컨테이너가 있다고 할 때 Functor는 이 컨테이너 안에 있는 값을 유지하면서 그 값을 변환하는 방법을 제공합니다. \
이를 통해 우리는 값을 안전하고 효율적으로 다룰 수 있습니다.

좀 더 수학적으로 말하자면, Functor는 특정 타입을 포함하고 있는 **매핑 가능한 구조**이며, 이를 변환하기 위해 `map`이라는 함수를 제공합니다. 이 `map` 함수는 원래의 컨테이너 구조는 유지하면서 그 안의 값을 변경합니다.

## 2. Functor의 종류

Functor는 여러 언어에서 다르게 구현되지만, 본질적인 역할은 동일합니다. 여기에서는 하스켈, 코틀린, 자바스크립트에서의 Functor 구현을 살펴보겠습니다.

### 2.1 Maybe Functor

#### Haskell에서의 Maybe

`Maybe`는 하스켈에서 가장 일반적인 Functor 중 하나로, 값이 있을 수도 없을 수도 있는 경우를 다루기 위해 사용됩니다. `Maybe`는 `Just`와 `Nothing` 두 가지로 구성되며, 이를 통해 값이 존재하지 않을 가능성을 안전하게 표현할 수 있습니다.

다음은 하스켈의 `Maybe` Functor 예제입니다:

```haskell
import Data.Maybe

example1 :: Maybe Int
example1 = Just 3

example2 :: Maybe Int
example2 = fmap (+1) example1  -- 결과: Just 4

example3 :: Maybe Int
example3 = fmap (+1) Nothing   -- 결과: Nothing
```

이처럼 `fmap`을 사용해 `Maybe` 타입 내부의 값을 안전하게 변환할 수 있습니다.

#### Kotlin에서의 Maybe Functor

Kotlin의 표준 라이브러리에는 하스켈의 `Maybe`와 완전히 동일한 Functor가 없습니다. Kotlin의 `Nullable` 타입은 비슷해 보이지만 실제로는 Functor의 법칙을 완벽히 만족하지 않습니다. 대신 Maybe Functor를 직접 구현하여 사용할 수 있습니다:

```kotlin
sealed class Maybe<out A> {
    data class Just<out A>(val value: A) : Maybe<A>()
    object Nothing : Maybe<Nothing>()

    // Functor의 map 구현
    inline fun <B> map(f: (A) -> B): Maybe<B> = when (this) {
        is Just -> Just(f(value))
        is Nothing -> Nothing
    }
}

// 사용 예시
fun main() {
    val maybeValue = Maybe.Just(3)
    val result = maybeValue.map { it + 1 }  // 결과: Just(4)

    val nothingValue = Maybe.Nothing
    val result2 = nothingValue.map { it + 1 }  // 결과: Nothing
}

#### JavaScript에서의 Maybe

JavaScript도 기본적으로 `Maybe` 타입이 없지만, `Maybe` Functor를 직접 만들어볼 수 있습니다.
이를 통해 값의 존재 여부에 따른 처리를 더 안전하게 할 수 있습니다.

```javascript
class Maybe {
  constructor(value) {
    this.value = value;
  }

  static of(value) {
    return new Maybe(value);
  }

  map(fn) {
    return this.value ? Maybe.of(fn(this.value)) : Maybe.of(null);
  }
}

const example1 = Maybe.of(3).map(x => x + 1); // 결과: Maybe { value: 4 }
const example2 = Maybe.of(null).map(x => x + 1); // 결과: Maybe { value: null }
```

### 2.2 Either Functor

#### Haskell에서의 Either

`Either`는 두 가지 가능성을 나타내는 타입입니다. `Left`는 오류나 실패를, `Right`는 정상적인 값을 나타냅니다. 이를 통해 오류 처리를 더 명시적으로 할 수 있습니다.

하스켈의 `Either` 예제는 다음과 같습니다:

```haskell
import Data.Either

example1 :: Either String Int
example1 = Right 10

example2 :: Either String Int
example2 = fmap (+5) example1  -- 결과: Right 15

example3 :: Either String Int
example3 = Left "Error occurred"
example4 = fmap (+5) example3   -- 결과: Left "Error occurred"
```

`Either`를 통해 오류 처리를 할 때 오류가 발생했을 경우, `fmap`은 `Left`의 값을 유지하고 아무런 변환도 하지 않습니다.

#### Kotlin에서의 Result

Kotlin에서는 `Result<T>`가 `Either`와 유사한 역할을 수행합니다. `Result`를 통해 성공(`Success`) 또는 실패(`Failure`)를 표현하고 안전하게 처리할 수 있습니다.

```kotlin
val result: Result<Int> = Result.success(10)
val newResult = result.map { it + 5 }  // 결과: Success(15)

val errorResult: Result<Int> = Result.failure(Exception("Error occurred"))
val newErrorResult = errorResult.map { it + 5 }  // 결과: Failure(java.lang.Exception: Error occurred)
```

`map` 함수를 통해 `Success` 상태인 경우에만 값을 변환하고, `Failure` 상태는 그대로 유지합니다.

#### JavaScript에서의 Either

JavaScript에서도 `Either` 타입을 직접 구현할 수 있습니다. 이를 통해 명시적으로 오류 처리를 수행할 수 있습니다.

```javascript
class Either {
  constructor(left, right) {
    this.left = left;
    this.right = right;
  }

  static left(value) {
    return new Either(value, null);
  }

  static right(value) {
    return new Either(null, value);
  }

  map(fn) {
    return this.right ? Either.right(fn(this.right)) : this;
  }
}

const success = Either.right(10).map(x => x + 5); // 결과: Either { left: null, right: 15 }
const failure = Either.left("Error occurred").map(x => x + 5); // 결과: Either { left: "Error occurred", right: null }
```

## 3. Functor의 법칙

Functor는 다음 두 가지 법칙을 반드시 만족해야 합니다:

1. **보존 법칙 (Identity Law)**: `fmap id == id`

   - Functor에 있는 값을 그대로 두는 연산을 적용할 때 Functor 자체가 변하지 않아야 합니다.

   - 즉 항등함수(즉 함수가 X−>X인 경우) 에 Functor를 통해서 매핑하면, 반환되는 펑터는 원래의 펑터와 같다.

   - **Haskell 코드 예시**:

   ```haskell
   example1 :: Maybe Int
   example1 = Just 5

   example2 = fmap id example1  -- 결과: Just 5
   ```

   **Kotlin 코드 예시**:

   ```kotlin
   val maybeValue: Int? = 5
   val result = maybeValue?.let { it }  // 결과: 5
   ```

   **JavaScript 코드 예시**:

   ```javascript
   const maybeValue = Maybe.of(5);
   const result = maybeValue.map(x => x); // 결과: Maybe { value: 5 }
   ```

2. **결합 법칙 (Composition Law)**: `fmap (f . g) == (fmap f) . (fmap g)`

   - 두 함수를 연속적으로 적용하는 것은 각각을 `fmap`으로 적용한 것과 같아야 합니다.
   - 두 함수를 합성한 함수의 매핑은 각 함수를 매핑한 결과를 합성한 것과 같다.

   **Haskell 코드 예시**:

   ```haskell
   f :: Int -> Int
   f x = x + 2

   g :: Int -> Int
   g x = x * 3

   example1 :: Maybe Int
   example1 = Just 5

   composed = fmap (f . g) example1  -- 결과: Just 17
   separate = (fmap f . fmap g) example1  -- 결과: Just 17
   ```

   **Kotlin 코드 예시**:

   ```kotlin
   val maybeValue: Int? = 5

   val f = { x: Int -> x + 2 }
   val g = { x: Int -> x * 3 }

   val composed = maybeValue?.let { f(g(it)) }  // 결과: 17
   val separate = maybeValue?.let(g)?.let(f)  // 결과: 17
   ```

   **JavaScript 코드 예시**:

   ```javascript
   const f = x => x + 2;
   const g = x => x * 3;

   const maybeValue = Maybe.of(5);

   const composed = maybeValue.map(x => f(g(x))); // 결과: Maybe { value: 17 }
   const separate = maybeValue.map(g).map(f); // 결과: Maybe { value: 17 }
   ```

이 두 법칙을 만족함으로써 Functor는 안전하고 일관된 방식으로 값을 변환할 수 있는 보장된 구조를 제공한다.


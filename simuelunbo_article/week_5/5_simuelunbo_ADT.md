# 대수적 데이터 타입 (ADT)

대수적 데이터 타입(Algebraic Data Types, ADT)은 함수형 프로그래밍과 타입 이론에서 매우 중요한 개념으로, 여러 타입을 결합하여 합성 타입을 만드는 데 사용됩니다. 이 글에서는 Haskell 예제를 통해 ADT를 설명하고, 이해를 돕기 위해 Kotlin 예제도 포함하였습니다.

## 대수적 데이터 타입이란?
ADT는 다른 타입들을 결합하여 만들어진 타입입니다. ADT는 주로 두 가지 유형으로 구분됩니다:

1. **곱 타입 (Product Types)**
2. **합 타입 (Sum Types)**

곱 타입과 합 타입을 사용하면 데이터를 정밀하고 표현력 있게 나타낼 수 있어, 소프트웨어 도메인 모델을 설계하는 데 있어 매우 중요합니다.

### 곱 타입 (Product Types)
곱 타입은 여러 값을 함께 가지는 타입을 나타냅니다. 여러 값을 한 번에 보유하는 타입으로, 객체나 클래스로 생각할 수 있습니다. 예를 들어, 한 사람이 이름과 나이를 동시에 가질 수 있는 것처럼 여러 속성을 하나의 타입으로 결합하는 것입니다.


#### Haskell 예제

Haskell에서 곱 타입은 여러 필드를 결합한 데이터 구조로 표현할 수 있습니다:

```haskell
data Person = Person { name :: String, age :: Int }
```

여기서 `Person`은 `name`과 `age`를 결합한 곱 타입입니다. `Person`의 카디널리티(가능한 값의 수)는 각 필드의 카디널리티의 곱으로 계산됩니다.

#### Kotlin 예제
Kotlin에서 곱 타입은 `data class`를 사용하여 표현할 수 있습니다:

```kotlin
data class Person(val name: String, val age: Int)
```

두 언어 모두에서 `Person` 타입은 `name`과 `age`라는 두 필드를 결합합니다. `name`의 값이 `age`의 값에 영향을 미치지 않으며, 두 값은 독립적인 구성 요소입니다.

#### 곱 타입을 사용할 때
곱 타입은 구성 요소들이 독립적이며 함께 존재할 수 있을 때 사용합니다. 예를 들어, `Person` 타입에서 `name`과 `age`는 서로 영향을 미치지 않는 별개의 데이터입니다.

### 합 타입 (Sum Types)
합 타입은 여러 값 중 하나를 선택할 수 있는 타입을 나타냅니다. 이를 태그된 유니온(tagged union), 유니온, 또는 변형(variant)이라고도 합니다. 한 번에 하나의 타입만 사용할 수 있으며, 어떤 타입이 사용되고 있는지 태그로 구분합니다.

#### Haskell 예제
Haskell에서 합 타입의 일반적인 예는 `Maybe` 타입입니다. 이는 선택적 값을 나타냅니다:

```haskell
data Maybe a = Nothing | Just a
```

`Maybe`는 값이 없음을 나타내는 `Nothing` 또는 값을 포함하는 `Just a`가 될 수 있는 합 타입입니다.

#### Kotlin 예제
Kotlin에서는 `sealed` 클래스를 사용하여 합 타입을 표현할 수 있습니다:

```kotlin
sealed class Option<out T>
object None : Option<Nothing>()
data class Some<out T>(val value: T) : Option<T>()
```

여기서 `Option`은 `None` 또는 `Some`이 될 수 있으며, 이는 Haskell의 `Maybe`와 유사합니다. Kotlin에서 `sealed` 클래스를 사용하면 패턴 매칭 시 모든 가능한 경우를 처리하도록 보장할 수 있습니다.

#### 합 타입을 사용할 때
합 타입은 여러 가지 서로 다른 옵션이 존재하지만 한 번에 하나만 유효할 때 사용합니다. 예를 들어, 다양한 도형을 표현할 때 사용할 수 있습니다:

```haskell
data Shape = Circle Float | Rectangle Float Float
```

이 경우, 도형은 반지름을 가진 `Circle`이거나, 너비와 높이를 가진 `Rectangle`일 수 있습니다.

### 패턴 매칭
합 타입을 사용하면 패턴 매칭을 통해 다양한 경우를 효과적으로 처리할 수 있습니다.

#### Haskell에서의 패턴 매칭
`Maybe` 타입을 사용하여 `Nothing`과 `Just` 값을 패턴 매칭으로 처리할 수 있습니다:

```haskell
handleMaybe :: Maybe Int -> String
handleMaybe Nothing = "No value"
handleMaybe (Just x) = "Value is " ++ show x
```

#### Kotlin에서의 패턴 매칭
Kotlin에서는 `when`을 사용하여 `sealed` 클래스에 대해 패턴 매칭을 수행할 수 있습니다:

```kotlin
fun handleOption(option: Option<Int>): String = when (option) {
    is None -> "No value"
    is Some -> "Value is ${option.value}"
}
```

`sealed` 클래스를 사용하면 모든 가능한 경우를 다루도록 보장되어 코드가 더 안전하고 오류가 적습니다.

### 곱 타입 vs 합 타입
곱 타입과 합 타입의 주요 차이점은, 곱 타입은 여러 독립적인 값을 결합하는 반면, 합 타입은 여러 가지 가능성 중 하나를 선택하는 것입니다. 어떤 타입을 사용할지는 표현하려는 값들 간의 관계에 따라 달라집니다:

- **곱 타입** (AND): 여러 필드가 함께 존재해야 할 때 사용합니다.
- **합 타입** (OR): 여러 형태 중 하나의 값을 나타낼 때 사용합니다.

### ADT의 실제 사용 사례
- **곱 타입**: 여러 필드를 하나의 일관된 단위로 결합하는 `Person` 같은 데이터 구조를 나타낼 때 사용합니다.
- **합 타입**: 성공 또는 실패와 같은 계산의 상태를 나타낼 때 사용합니다 (`Haskell`의 `Either`나 `Kotlin`의 `Result`처럼).

## 결론
대수적 데이터 타입(ADT)은 함수형 스타일로 데이터를 모델링하는 강력한 방법으로, 값들 간의 관계를 표현력 있게 캡처할 수 있는 방법을 제공합니다. 곱 타입은 여러 값을 결합하고, 합 타입은 여러 값 중 하나를 선택할 수 있게 합니다. 이러한 개념들은 Haskell, Kotlin, TypeScript와 같은 언어에서 견고하고 표현력 있는 타입 시스템의 기반을 형성합니다.


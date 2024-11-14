# Haskell의 타입 클래스와 Kotlin에서의 활용

타입 클래스(Type Class)는 Haskell에서 등장한 개념으로, ad hoc 다형성을 지원하는 타입 시스템 구조입니다. 여기서 'ad hoc'이란 라틴어로 특정한 목적을 위해 일시적으로 정의된 것을 의미하며, Haskell에서는 오버로딩을 통해 같은 이름의 연산자나 함수가 다양한 타입에서 다르게 동작할 수 있도록 합니다. 간단히 말하면, 타입 클래스는 다양한 타입에 대해 공통된 행위를 정의하고 이를 특정 타입에 맞게 구현할 수 있는 방식입니다.

## Haskell의 타입 클래스 예시: `Show`

타입 클래스 `Show`는 어떤 타입을 사람이 읽을 수 있는 형태의 문자열로 변환하는 역할을 합니다. 예를 들어, `Show` 타입 클래스를 정의하고 이를 구현하는 방식은 다음과 같습니다.

```haskell
class Show a where
  show :: a -> String


instance Show Int where
  show x = "Int: " ++ Prelude.show x

instance Show Bool where
  show True = "Yes"
  show False = "No"
```

위 예시에서 `Show` 타입 클래스는 타입 `a`를 문자열로 변환하는 `show` 함수를 정의합니다. `Int`와 `Bool` 타입에 대한 `Show` 인스턴스를 정의하여, 각각의 타입이 문자열로 어떻게 표현되는지 구체화할 수 있습니다. 이를 통해 개발자는 다양한 타입을 동일한 방식으로 처리할 수 있습니다.

## Kotlin에서의 유사 개념: 인터페이스와 Arrow 라이브러리

Kotlin에서는 Haskell의 타입 클래스와 유사한 개념을 구현하기 위해 인터페이스와 Arrow 라이브러리를 사용할 수 있습니다. 각각의 접근 방식을 이해하면, 타입 클래스의 개념을 Kotlin에서 어떻게 흉내 낼 수 있는지 알 수 있습니다.

### 1. 인터페이스를 사용한 구현

Kotlin에서는 타입 클래스와 비슷한 기능을 제공하기 위해 인터페이스를 활용할 수 있습니다. 인터페이스는 클래스가 특정 행위를 구현하도록 명시하며, 이를 통해 타입에 따라 다양한 기능을 정의할 수 있습니다.

예를 들어, Haskell의 `Show`와 유사한 구조를 Kotlin에서 인터페이스로 구현하는 방식은 다음과 같습니다.

```kotlin
interface Show<T> {
    fun show(x: T): String
}

class ShowInt : Show<Int> {
    override fun show(x: Int): String = "Int: $x"
}

class ShowBool : Show<Boolean> {
    override fun show(x: Boolean): String = if (x) "Yes" else "No"
}
```

이 코드에서 `Show`라는 인터페이스를 정의하고, 이를 상속받아 `Int`와 `Boolean` 타입에 대해 각각의 표현 방식을 정의했습니다. 이는 Haskell의 `Show` 타입 클래스와 유사한 구조로, 특정 타입에 대한 문자열 표현을 제공할 수 있습니다. 하지만 완벽하게 Haskell 같이 구현은 하지는 못합니다.



#### 2. 재사용 가능한 상위 인터페이스 정의

특정 타입에 대해 반복되는 기능을 줄이기 위해 상위 인터페이스를 정의를 합니다. `Int` 타입에 대한 `Show` 기능을 미리 정의한 상위 인터페이스를 사용할 수 있습니다.

```kotlin
interface IntShow : Show<Int> {
    override fun show(x: Int): String = "Int: $x"
}

class SomeInt : IntShow {
    fun doSomething() {
        // 특정 로직 구현
    }
}
```

이러한 접근 방식은 코드 중복을 줄이고, 여러 클래스에서 공통적인 기능을 쉽게 재사용할 수 있게 합니다.

#### **3. 제네릭 함수와 매개변수화된 타입 사용**

제네릭을 사용하여 타입 클래스를 흉내 내는 것입니다. 제네릭 함수와 타입 매개변수를 사용하여 다양한 타입에 대해 동일한 기능을 제공할 수 있습니다:

```kotlin
fun <T> printShow(value: T, showInstance: Show<T>) {
    println(showInstance.show(value))
}

val showIntInstance = ShowInt()
val showBoolInstance = ShowBool()

printShow(42, showIntInstance)   // 출력: Int: 42
printShow(true, showBoolInstance) // 출력: Yes
```

이 방식은 타입 클래스를 매개변수로 받아들이는 함수로 다양한 타입에 대해 동일한 인터페이스를 사용할 수 있도록 해줍니다. 이는 Haskell의 타입 클래스와 유사한 기능을 제공하지만 하스켈에 비하여 코드 길이는 길어집니다.

## Arrow를 활용한 타입 클래스 구현

Kotlin의 Arrow 라이브러리는 함수형 프로그래밍을 지원하는 다양한 기능을 제공하며Haskell의 타입 클래스 개념을 보다 자연스럽게 흉내 낼 수 있는 방법을 제공합니다.

예를 들어 `Show`와 비슷한 예시를 들자면

```kotlin
import arrow.core.*

interface Show<A> {
    fun show(value: A): String
}

object ShowInt : Show<Int> {
    override fun show(value: Int): String = "Int: $value"
}

object ShowBool : Show<Boolean> {
    override fun show(value: Boolean): String = if (value) "Yes" else "No"
}

fun <A> display(showInstance: Show<A>, value: A) {
    println(showInstance.show(value))
}

display(ShowInt, 10)    // 출력: Int: 10
display(ShowBool, true) // 출력: Yes
```

Arrow를 사용하면 타입 클래스의 패턴을 활용하여 다양한 데이터 구조에 대해 공통적인 연산을 정의하고 사용할 수 있습니다.\
위 코드 interface 1\~3번 방식과 유사 합니다.\
이는 코드의 일관성과 재사용성을 크게 높여줍니다. 특히 함수형 프로그래밍에서 흔히 사용되는 패턴을 제공하므로, 함수형 사고 방식을 Kotlin에서도 쉽게 적용할 수 있도록 도와줍니다.

## 정리

타입 클래스와 인터페이스는 모두 다형성을 지원하는 중요한 개념입니다.

타입 클래스와 인터페이스는 행위에 대한 선언만하고 실제 구현은 상속받은 클래스가 구현하는 점은 같습니다. 하지만 Haskell의 타입 클래스는 특정 행위를 선언하고 이를 타입에 암시적으로 적용할 수 있는 반면, Kotlin의 인터페이스는 명시적으로 행위를 상속받아 구현해야 합니다.

코틀린에서 Arrow 라이브러리는 이러한 타입 클래스 개념을 더욱 자연스럽게 Kotlin에 적용할 수 있어 보다 함수형 프로그래밍 스타일을 구현할 수 있습니다.

확장 함수와 상위 인터페이스를 활용하면 Kotlin에서도 타입 클래스의 개념을 어느 정도 흉내 낼 수 있습니다.

다만, Haskell의 타입 클래스처럼 완전히 암시적으로 적용되는 구조를 만들기는 어렵습니다.

함수형 프로그래밍 개념을 이해하고 적절히 활용하는 것은 코드의 일관성과 유지보수성을 높이는 중요한 방법입니다. 각 접근 방식의 장단점을 이해하고 상황에 맞는 적절한 방법을 선택하는 것이 좋습니다.


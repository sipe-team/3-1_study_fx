# Free Monad란 무엇인가?

**Free Monad**는 Functor로부터 모나드를 생성하여 프로그램의 연산을 **데이터로 표현**하고, 나중에 원하는 방식으로 **해석(실행)** 할 수 있게 해줍니다. 이를 통해 실행 방법을 나중에 결정할 수 있어 **유연성**과 **모듈성**이 향상됩니다.

### Free Monad의 구조
Free Monad는 Functor로부터 모나드를 생성합니다. 이를 통해 프로그램의 연산을 데이터로 취급하고, 나중에 원하는 방식으로 해석(실행)할 수 있습니다.

#### 구조
```
Functor -> Monad -> Monad -> ... -> Functor
```
- Functor를 연속적으로 모나드로 감싸면서 연산의 순서를 정의하고, 최종적으로 Functor로 돌아옵니다.



## 간단한 Free Monad 예제

키-값 저장소(Key-Value Store)를 조작하는 간단한 DSL(Domain Specific Language)을 만들어보겠습니다.

### Haskell에서의 구현

우선, 키-값 저장소에 대한 연산을 Functor로 정의합니다.

```haskell
{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.Free

data KVStoreF next
    = Put String Int next
    | Get String (Int -> next)
    | Delete String next
    deriving (Functor)
```

- **KVStoreF**: 키-값 저장소에 대한 연산을 정의한 Functor입니다.
- **DeriveFunctor**: Functor 인스턴스를 자동으로 유도하기 위한 언어 확장입니다.

#### 2. 리프팅 함수

각 연산을 Free Monad로 리프팅하기 위한 함수를 정의합니다.

```haskell
type KVStore = Free KVStoreF

put :: String -> Int -> KVStore ()
put key value = liftF $ Put key value ()

get :: String -> KVStore Int
get key = liftF $ Get key id

delete :: String -> KVStore ()
delete key = liftF $ Delete key ()
```

- **liftF**: Functor에서 Free Monad로 값을 들어 올립니다.
- **id**: `Get` 연산에서 결과를 다음 연산으로 전달하기 위한 함수입니다.

#### 3. 프로그램 작성

Free Monad를 사용하여 키-값 저장소를 조작하는 프로그램을 작성합니다.

```haskell
program :: KVStore (Maybe Int)
program = do
    put "wild-cats" 2
    put "tame-cats" 5
    n <- get "wild-cats"
    delete "tame-cats"
    return (Just n)
```

- `"wild-cats"` 키에 `2`를 저장합니다.
- `"tame-cats"` 키에 `5`를 저장합니다.
- `"wild-cats"` 키에서 값을 가져와 `n`에 저장합니다.
- `"tame-cats"` 키를 삭제합니다.
- 최종적으로 `n`을 `Just`로 감싸 반환합니다.

#### 4. 인터프리터 작성

프로그램을 실제로 실행하기 위한 인터프리터를 작성합니다.

```haskell
import Control.Monad.State
import qualified Data.Map as M

type KVStoreState = M.Map String Int

runKVStore :: KVStore a -> State KVStoreState a
runKVStore (Pure a) = return a
runKVStore (Free (Put key value next)) = do
    modify (M.insert key value)
    runKVStore next
runKVStore (Free (Get key f)) = do
    store <- get
    let value = M.findWithDefault 0 key store
    runKVStore (f value)
runKVStore (Free (Delete key next)) = do
    modify (M.delete key)
    runKVStore next
```

- **State 모나드**를 사용하여 상태를 유지합니다.
- 각 연산에 대해 실제 동작을 정의하고, 상태를 업데이트합니다.

#### 5. 실행

프로그램을 실행하고 결과를 확인합니다.

```haskell
main :: IO ()
main = do
    let initialState = M.empty
        (result, finalState) = runState (runKVStore program) initialState
    print result          -- 출력: Just 2
    print finalState      -- 출력: fromList [("wild-cats",2)]
```

- `runState`를 통해 프로그램을 실행하고, 결과와 최종 상태를 얻습니다.
- 결과는 `Just 2`이며, `"wild-cats"` 키에 `2`가 저장된 것을 확인할 수 있습니다.

### Kotlin에서의 구현

Kotlin은 **Higher-Kinded Types(HKT)**를 지원하지 않기 때문에, **Arrow 라이브러리**를 사용하여 Free Monad를 구현합니다.

🚀 Higher-Kinded Types(HKT)란?
> 타입을 인자로 받는 타입을 의미 하는 것으로, 예를 들어 `List<A>`는 `A`를 인자로 받는 타입이라고 할 수 있습니다.
Scala나 Haskell은 이러한 HKT를 자연스럽게 지원하여 F[_] 같은 추상화를 할 수 있습니다.

설명만 들어도 아직 이해하기가 어려운데 대략적인 예제 코드를 보면서 이해하자면 다음 코드와 같다.

```kotlin
// HKT가 없다면 이런 식으로 각각의 타입에 대해 별도 구현이 필요합니다
interface ListMonad {
    fun <A, B> map(list: List<A>, f: (A) -> B): List<B>
}

interface OptionMonad {
    fun <A, B> map(option: Option<A>, f: (A) -> B): Option<B>
}

// HKT가 있다면 이렇게 한번에 추상화할 수 있습니다 (의사 코드)
interface Monad<F> {
    fun <A, B> map(fa: F<A>, f: (A) -> B): F<B>
}
```

#### 1. 문법 정의

키-값 저장소에 대한 연산을 Functor로 정의합니다.

```kotlin
import arrow.Kind
import arrow.free.Free
import arrow.free.free
import arrow.higherkind

@higherkind
sealed class KVStoreF<out A> : KVStoreFOf<A> {
    data class Put(val key: String, val value: Int) : KVStoreF<Unit>()
    data class Get(val key: String) : KVStoreF<Int>()
    data class Delete(val key: String) : KVStoreF<Unit>()
}
```

- **@higherkind**: Arrow에서 HKT를 에뮬레이션하기 위한 애노테이션입니다.
- **KVStoreF**: 키-값 저장소에 대한 연산을 정의한 Functor입니다.

#### 2. 리프팅 함수

각 연산을 Free Monad로 리프팅하기 위한 함수를 정의합니다.

```kotlin
typealias KVStore<A> = Free<KVStoreFPartialOf, A>

fun put(key: String, value: Int): KVStore<Unit> = Free.liftF(KVStoreF.Put(key, value))
fun get(key: String): KVStore<Int> = Free.liftF(KVStoreF.Get(key))
fun delete(key: String): KVStore<Unit> = Free.liftF(KVStoreF.Delete(key))
```

#### 3. 프로그램 작성

Free Monad를 사용하여 프로그램을 작성합니다.

```kotlin
val program: KVStore<Int> = free {
    put("wild-cats", 2).bind()
    put("tame-cats", 5).bind()
    val n = get("wild-cats").bind()
    delete("tame-cats").bind()
    n
}
```

- **free { ... }** 블록 안에서 연산을 순차적으로 수행합니다.
- **bind()**를 사용하여 각 연산의 결과를 추출합니다.

#### 4. 인터프리터 작성

프로그램을 실제로 실행하기 위한 인터프리터를 작성합니다.

```kotlin
import arrow.core.Id
import arrow.core.extensions.id.monad.monad
import arrow.free.extensions.free.foldMap
import arrow.typeclasses.FunctionK

val interpreter = object : FunctionK<KVStoreFPartialOf, Id> {
    private val kvs = mutableMapOf<String, Int>()

    override fun <A> invoke(fa: Kind<KVStoreFPartialOf, A>): Id<A> {
        val op = fa.fix()
        return when (op) {
            is KVStoreF.Put -> {
                println("put(${op.key}, ${op.value})")
                kvs[op.key] = op.value
                Id(Unit as A)
            }
            is KVStoreF.Get -> {
                println("get(${op.key})")
                Id((kvs[op.key] ?: 0) as A)
            }
            is KVStoreF.Delete -> {
                println("delete(${op.key})")
                kvs.remove(op.key)
                Id(Unit as A)
            }
        }
    }
}
```

- **FunctionK**를 구현하여 `KVStoreF`를 `Id`로 변환합니다.
- 각 연산에 대해 실제 동작을 정의합니다.

#### 5. 실행

프로그램을 실행하고 결과를 확인합니다.

```kotlin
fun main() {
    val result = program.foldMap(interpreter, Id.monad())
    println(result) // 출력: Id(2)
}
```

- **foldMap**을 사용하여 프로그램을 실행합니다.
- 결과는 `Id(2)`로, `"wild-cats"` 키에 저장된 값 `2`를 반환합니다.

## Free Monad의 활용과 가치

### 실행 방법의 유연성

Free Monad를 사용하면 프로그램의 연산을 **데이터로 표현**하여 **실행 방법을 나중에 결정**할 수 있습니다.

- **다양한 인터프리터**를 정의하여 동일한 프로그램을 다른 방식으로 실행할 수 있습니다.
- **테스트** 시에는 모의(Mock) 인터프리터를 사용하여 부작용 없이 동작을 검증할 수 있습니다.

### DSL로서의 가치

- **도메인에 특화된 언어**를 정의하여 비즈니스 로직을 명확하게 표현할 수 있습니다.
- 프로그램의 구조와 실행을 분리하여 **코드의 재사용성과 모듈성**을 높일 수 있습니다.

## IO Monad와의 비교

### IO Monad란 무엇인가?

**IO Monad**는 부작용이 있는 연산을 안전하게 다루기 위한 모나드입니다.

- **연산의 표현과 실행 시점**을 분리하지만, **실행 방법은 고정**되어 있습니다.
- 주로 **입출력(IO) 연산**을 처리하는 데 사용됩니다.

### Free Monad의 장점

- **실행 방법의 유연성**: 다양한 인터프리터를 통해 실행 방법을 변경할 수 있습니다.
- **테스트 용이성**: 실제 IO를 수행하지 않고도 프로그램을 테스트할 수 있습니다.
- **확장성**: 새로운 연산을 추가하거나 변경하기 쉽습니다.




프로젝트의 요구 사항과 팀의 역량에 따라 Free Monad를 도입하면 **코드의 유연성**, **모듈성**, **테스트 용이성**을 높일 수 있습니다. 그러나 개념의 복잡성과 학습 곡선을 고려하여 신중하게 접근하는 것이 좋습니다.


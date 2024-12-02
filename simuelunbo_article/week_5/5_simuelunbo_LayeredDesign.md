# 함수형 프로그래밍 계층형 설계 

계층형 설계는 소프트웨어 코드를 특정 기능과 책임에 따라 구분된 계층으로 조직화하는 접근 방식입니다. 이 방법은 코드를 수정, 읽기, 테스트 및 재사용하기 쉽게 만드는 것을 목표로 합니다. 계층을 통해 관심사를 분리하고 복잡성을 효과적으로 관리할 수 있습니다.

다음은 계층형 설계의 예시

```
계층형 설계
├── cartTax()          // 비즈니스 규칙
│
├── add_item()         // 기능적 동작
│
├── add_element_last() // Copy-on-write 메커니즘
│
└── .slice()           // JavaScript에서 제공하는 배열 조작
```

이 구조에서:

- `cartTax()`는 비즈니스 로직을 처리합니다.
- `add_item()`은 기능적 작업을 관리합니다.
- `add_element_last()`는 copy-on-write 메커니즘을 구현합니다.
- `.slice()`는 JavaScript의 내장 배열 메서드입니다.

각 계층은 아래에 있는 계층을 기반으로 구축되며, 하위 세부 사항을 추상화합니다. 이러한 계층을 효과적으로 식별하고 활용하는 것이 유지 보수성과 확장성이 높은 소프트웨어를 만드는 열쇠입니다.

계층형 설계에는 네 가지 주요 패턴이 있습니다:

1. **직접 구현 (Direct Implementation)**
2. **추상화 벽 (Abstraction Barrier)**
3. **작은 인터페이스 (Small Interfaces)**
4. **편리한 계층 (Convenient Layers)**

## 1. 직접 구현 (Direct Implementation)

직접 구현에서는 함수 시그니처가 나타내는 문제를 함수 본문에서 구체적으로 실현합니다. 각 계층은 자신의 책임을 직접 수행하며, **바로 아래 계층만 알고 있어야 합니다**. 다른 계층의 구체적인 구현을 알 필요가 없으며, 의존성을 최소화하고 재사용성을 높이는 것이 목표입니다.

예를 들어, 회사의 계층 구조는 다음과 같습니다:

```
사장 -> 부장 -> 과장 -> 대리
```

각 역할은 자신 아래 직급의 업무만 알면 되며, 상세한 작업을 알 필요는 없습니다. 마찬가지로, 소프트웨어에서도 각 함수는 바로 아래 계층의 함수만 알고 있어야 합니다.

### Haskell에서의 예시

```haskell
-- 데이터 처리 함수 (상위 계층)
processData :: [Int] -> Int
processData xs = sumList (incrementList xs)

-- 리스트의 각 요소를 증가시키는 함수 (중간 계층)
incrementList :: [Int] -> [Int]
incrementList xs = map increment xs

-- 요소를 증가시키는 함수 (하위 계층)
increment :: Int -> Int
increment x = x + 1

-- 리스트의 합계를 계산하는 함수 (하위 계층)
sumList :: [Int] -> Int
sumList xs = sum xs
```

각 함수는 바로 아래 계층의 함수만 호출하며, 세부 구현에 대해 알 필요가 없습니다.

### JavaScript에서의 예시

```javascript
// 상위 계층 함수: 데이터를 처리합니다.
function processData(data) {
  return sumArray(incrementArray(data));
}

// 중간 계층 함수: 배열의 각 요소를 증가시킵니다.
function incrementArray(arr) {
  return arr.map(increment);
}

// 하위 계층 함수: 값을 증가시킵니다.
function increment(x) {
  return x + 1;
}

// 하위 계층 함수: 배열의 합계를 계산합니다.
function sumArray(arr) {
  return arr.reduce((acc, curr) => acc + curr, 0);
}
```

각 함수는 바로 아래 계층의 함수만 호출하며, 다른 함수의 내부 작동을 알 필요가 없습니다.

### 코틀린에서의 예시

코틀린에서는 계층의 개념을 함수의 구성과 역할 분리로 이해할 수 있습니다. 각 계층을 구분하여 함수들이 서로의 세부 구현을 알 필요 없이 작업을 수행합니다.

```kotlin
// 상위 계층 함수: 주문을 처리합니다.
fun processOrder(order: Order): Receipt {
    val discountedOrder = applyDiscount(order)
    return generateReceipt(discountedOrder)
}

// 중간 계층 함수: 할인 적용
fun applyDiscount(order: Order): Order {
    return if (order.total >= 100.0) {
        order.copy(total = order.total * 0.9)
    } else {
        order
    }
}

// 하위 계층 함수: 영수증 생성
fun generateReceipt(order: Order): Receipt {
    return Receipt(items = order.items, total = order.total)
}

// 데이터 클래스들
data class Order(val items: List<Item>, val total: Double)
data class Item(val name: String, val price: Double)
data class Receipt(val items: List<Item>, val total: Double)
```

각 함수는 바로 아래 계층의 함수만 호출하며, 각자의 역할에 집중합니다.

### 핵심 포인트

- 각 계층은 바로 아래 계층만 알고 있어야 합니다.
- 의존성을 최소화하고 재사용성을 높입니다.
- 인터페이스에 의존함으로써 상위 계층은 하위 계층의 구체적인 구현에 의존하지 않습니다.

### Question
하위 계층의 코드(즉 core)가 잘못 설계되면 상위 계층까지 연쇄적으로 수정이 필요한 상황이 발생 할 수 있지 않을까?

#### 위 문제 해결방안
- 설계 결함이 하위 계층에 있으면 상위 계층에 영향을 미칠 수 있으므로, **인터페이스 분리**와 **의존성 역전**을 통해 이를 완화합니다.

1. 인터페이스 분리 원칙 (Interface Segregation Principle) -> 3번 작은 인터페이스와 의미 하는 바가 같습니다.
   - 큰 인터페이스를 작은 단위로 분리
   - 필요한 기능만 노출
2. 의존성 역전 원칙 (Dependency Inversion Principle)
   - 구체적인 구현체가 아닌 추상화(인터페이스)에 의존
   - 모든 계층이 안쪽 방향의 인터페이스를 바라보도록 설계

```kotlin
// Kotlin
// 1. 인터페이스 분리
interface OrderCreator {
    fun createOrder(request: OrderRequest): Order
}

interface OrderFinder {
    fun findById(id: String): Order?
}

// 2. 의존성 역전
interface OrderRepository {
    fun save(order: Order)
    fun findById(id: String): Order?
}
// OrderRepository 인터페이스의 구현 클래스와 그 구현체를 OrderService에 주입 하는 코드는 생략

class OrderService(
    private val orderRepository: OrderRepository  // 구체적 구현체가 아닌 인터페이스에 의존 해당 인터페이스와 구현체 연결 코드는 생략
) : OrderCreator, OrderFinder {
    override fun createOrder(request: OrderRequest): Order {
        val order = Order.from(request)
        return orderRepository.save(order)
    }
    
    override fun findById(id: String): Order? {
        return orderRepository.findById(id)
    }
}
```

### 직접 구현을 위한 체크리스트

- 이 추상화가 정말 필요한가?
- 향후 구현이 변경될 가능성이 있는가?
- 테스트를 위해 목(mock)이 필요한가?

## 2. 추상화 벽 (Abstraction Barrier)

추상화 벽은 구현 세부 사항을 숨기는 함수들로 구성된 계층입니다. 이 계층의 함수를 사용할 때는 내부 구현을 알 필요 없이 입력과 출력만 알면 됩니다.

### Haskell에서의 예시

```haskell
-- 추상화 벽: 데이터베이스 접근을 추상화합니다.
getUser :: UserID -> IO User
getUser uid = queryDatabase ("SELECT * FROM users WHERE id = " ++ show uid)

-- 상위 계층 함수: 사용자 프로필을 가져옵니다.
getUserProfile :: UserID -> IO UserProfile
getUserProfile uid = do
    user <- getUser uid
    return (UserProfile (userName user) (userAge user))
```

`getUser` 함수는 데이터베이스 접근의 세부 사항을 숨기며, 상위 계층 함수는 이를 신경 쓸 필요가 없습니다.

### JavaScript에서의 예시

```javascript
// 추상화 벽: 데이터베이스 API
function getUser(id) {
  // 구현 세부 사항은 숨겨져 있습니다.
  return database.query(`SELECT * FROM users WHERE id = ${id}`);
}

// 상위 계층 함수: 사용자 프로필을 가져옵니다.
function getUserProfile(id) {
  const user = getUser(id);
  return {
    name: user.name,
    age: user.age,
  };
}
```

`getUser` 함수는 데이터베이스 접근의 세부 사항을 숨기며, 상위 계층 함수는 이를 알 필요가 없습니다.

### 주의사항

- 인터페이스를 과도하게 생성하지 말 것: 관련된 기능을 그룹화합니다.
- 실제로 필요한 추상화만 할 것: 단순한 기능을 위해 불필요한 추상화를 만들지 않습니다.
- 한 인터페이스에 너무 많은 메서드를 담지 말 것: 인터페이스는 단일 책임을 가져야 합니다.

### 추상화 벽을 사용하기 좋은 시기

1. 구현을 쉽게 변경하기 위해
2. 코드를 읽고 쓰기 쉽게 만들기 위해
3. 팀 간의 조율을 줄이기 위해
4. 주어진 문제에 집중하기 위해

## 3. 작은 인터페이스 (Small Interfaces)

작은 인터페이스는 비즈니스 개념을 나타내는 강력한 동작의 최소 집합으로 구성됩니다. 인터페이스를 작게 유지하는 것은 여러 가지 이점이 있습니다.

### Haskell에서의 예시

```haskell
-- 작은 인터페이스 정의: 재고 관리 서비스
class InventoryService s where
    addItem :: s -> Item -> s
    removeItem :: s -> ItemID -> s

-- 서비스 구현체
data Inventory = Inventory { items :: [Item] }

instance InventoryService Inventory where
    addItem inventory item = inventory { items = item : items inventory }
    removeItem inventory itemId = inventory { items = filter ((/= itemId) . itemID) (items inventory) }

-- 아이템 데이터 타입
data Item = Item { itemID :: Int, itemName :: String, itemPrice :: Double }

```
- `InventoryService` 인터페이스는 `addItem`과 `removeItem` 두 가지 메서드만 포함하고 있습니다.
- 인터페이스를 작게 유지하여 이해하기 쉽고 구현하기도 간단합니다.

### JavaScript에서의 예시
 주문 처리 시스템에서 주문을 추가하고 취소하는 작은 인터페이스를 생각해봅시다.
```javascript
// 작은 인터페이스: 주문 서비스
class OrderService {
  constructor() {
    this.orders = [];
  }

  // 주문 추가
  addOrder(order) {
    this.orders.push(order);
  }

  // 주문 취소
  cancelOrder(orderId) {
    this.orders = this.orders.filter(order => order.id !== orderId);
  }
}

// 사용 예시
const orderService = new OrderService();

// 주문 추가
orderService.addOrder({ id: 1, items: ['노트북', '마우스'], total: 1600 });
orderService.addOrder({ id: 2, items: ['키보드'], total: 100 });

// 주문 취소
orderService.cancelOrder(1);

// 현재 주문 목록 출력
console.log(orderService.orders);
// 출력: [{ id: 2, items: ['키보드'], total: 100 }]

```
- `OrderService` 클래스는 `addOrder`와 `cancelOrder` 메서드만을 가지고 있습니다.
- `addOrder`: 새로운 주문을 주문 목록에 추가합니다.
- `cancelOrder`: 특정 ID를 가진 주문을 주문 목록에서 제거합니다.
- 인터페이스가 작고 단순하여 이해하기 쉽고 사용하기 편리합니다.

### 작은 인터페이스를 유지해야 하는 이유

1. 변경이 용이함: 메서드가 적을수록 수정해야 할 코드가 적습니다.
2. 버그 감소: 코드량이 적으면 버그 발생 가능성이 줄어듭니다.
3. 이해가 쉬움: 작은 인터페이스는 이해하기 쉽습니다.
4. 조율 감소: 팀 간 독립성이 높아집니다.
5. 사용의 용이성: 구현하고 사용하기가 쉽습니다.

## 4. 편리한 계층 (Convenient Layers)

편리한 계층은 실용성에 초점을 둡니다. 현재 작성 중인 코드가 편리하지 않다면, 설계를 다시 고려해야 합니다. 코드가 편리하게 느껴진다면, 더 이상의 변경 없이 진행할 수 있습니다.

생산성과 코드 품질을 향상시키는 계층에 시간을 투자하세요. 핵심은 개발을 더욱 효율적이고 유지 보수하기 쉽게 만드는 것입니다.


## 계층형 설계 사용하게 되면?

- **유지 보수성**: 상위로 연결된 것이 적은 함수일수록 변경하기 쉽습니다.
- **테스트 가능성**: 상위 계층의 함수는 인터페이스를 사용하여 테스트할 수 있습니다.
- **재사용성**: 하위 계층의 함수가 적을수록 재사용성이 높아집니다.

계층형 설계 원칙을 이해하고 적용하면, 조직화된 코드를 작성하여 유지 보수, 테스트 및 팀 간의 협업을 더욱 쉽게 할 수 있습니다.
# Haskell drawCard 함수 코드 분석

아래 코드는 게임 상태에서 카드 한 장을 뽑는 함수의 구현을 분석합니다:

```haskell
drawCard :: GameState -> (Card, GameState)
drawCard gs@(GameState ph dh (c:cs)) = (c, gs { deck = cs })
```

## 1. 함수 시그니처 분석

```haskell
drawCard :: GameState -> (Card, GameState)
```

- **입력**: 현재 게임 상태 (GameState)
- **출력**: 뽑은 카드와 새로운 게임 상태의 튜플
- **순수 함수**: IO를 사용하지 않는 순수 함수

## 2. 문법적 요소 분석

### 2.1 @ 패턴 (as-pattern)

```haskell
gs@(GameState ph dh (c:cs))
```

- `@` 기호: as-pattern을 나타냄
- `gs`: 전체 GameState를 이 이름으로 참조
- 패턴 매칭과 동시에 전체 값에 대한 참조를 얻을 수 있음

### 2.2 패턴 매칭

```haskell
(GameState ph dh (c:cs))
```

- `GameState`: 생성자 패턴 매칭
- `ph`: playerHand 필드
- `dh`: dealerHand 필드
- `(c:cs)`: deck 필드의 리스트 패턴 매칭
  - `c`: 첫 번째 카드
  - `cs`: 나머지 카드들
  - `:`: 리스트 cons 연산자

### 2.3 레코드 업데이트 구문

```haskell
gs { deck = cs }
```

- `gs`: 기존 GameState
- `{ deck = cs }`: deck 필드만 cs로 업데이트
- 다른 필드들(playerHand, dealerHand)은 변경되지 않음

## 3. 함수 동작 분석

### 3.1 실행 과정

```haskell
-- 입력 예시
gs = GameState
    { playerHand = [Card Ace Hearts]
    , dealerHand = [Card King Diamonds]
    , deck = [Card Two Spades, Card Three Clubs, ...]
    }

-- drawCard 실행 결과
(Card Two Spades,  -- 뽑은 카드
 GameState         -- 새로운 게임 상태
    { playerHand = [Card Ace Hearts]
    , dealerHand = [Card King Diamonds]
    , deck = [Card Three Clubs, ...]
    })
```

### 3.2 상세 동작

1. 패턴 매칭으로 덱의 첫 카드(`c`)와 나머지 카드들(`cs`) 분리
2. 튜플의 첫 요소로 뽑은 카드(`c`) 반환
3. 튜플의 두 번째 요소로 덱이 업데이트된 새 게임 상태 반환

## 4. 사용 예시

### 4.1 기본 사용

```haskell
-- 카드 뽑기
let (drawnCard, newState) = drawCard gameState
```

### 4.2 플레이어 카드 추가

```haskell
addCardToPlayer :: GameState -> (Card, GameState)
addCardToPlayer gs =
    let (card, newGs) = drawCard gs
    in (card, newGs { playerHand = card : playerHand newGs })
```

### 4.3 딜러 카드 추가

```haskell
addCardToDealer :: GameState -> (Card, GameState)
addCardToDealer gs =
    let (card, newGs) = drawCard gs
    in (card, newGs { dealerHand = card : dealerHand newGs })
```

## 5. 패턴 매칭의 안전성

### 5.1 빈 덱 처리 (현재 구현에서는 누락됨)

```haskell
-- 더 안전한 버전
drawCard :: GameState -> Maybe (Card, GameState)
drawCard (GameState ph dh []) = Nothing
drawCard gs@(GameState ph dh (c:cs)) =
    Just (c, gs { deck = cs })
```

### 5.2 완전한 패턴 매칭

```haskell
drawCard :: GameState -> (Card, GameState)
drawCard gs@(GameState ph dh (c:cs)) = (c, gs { deck = cs })
drawCard _ = error "Empty deck"  -- 또는 Maybe 타입 사용
```

## 6. 문법적 대안들

### 6.1 where 절 사용

```haskell
drawCard :: GameState -> (Card, GameState)
drawCard gameState = (card, newState)
    where
        card = head (deck gameState)
        newState = gameState { deck = tail (deck gameState) }
```

### 6.2 do 표기법 (Maybe 모나드 사용 시)

```haskell
drawCard :: GameState -> Maybe (Card, GameState)
drawCard gameState = do
    card <- listToMaybe (deck gameState)
    let newDeck = tail (deck gameState)
    return (card, gameState { deck = newDeck })
```

## 7. 주요 특징

1. **불변성**

   - 원본 GameState를 변경하지 않고 새로운 상태 반환
   - 함수형 프로그래밍의 핵심 원칙 준수

2. **패턴 매칭**

   - 복잡한 데이터 구조를 효율적으로 분해
   - 코드의 가독성과 안전성 향상

3. **타입 안전성**
   - 컴파일 시점에 많은 오류 방지
   - 명확한 함수 시그니처

이 함수는 간단하지만 Haskell의 여러 핵심 기능을 효과적으로 활용하고 있습니다.

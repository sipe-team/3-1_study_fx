# 블랙잭 게임 타입 정의 분석

## 1. 카드 구성 요소 정의

### 1.1 Suit (무늬) 정의

```haskell
data Suit = Hearts | Diamonds | Clubs | Spades
    deriving (Show, Eq)
```

- **값 생성자들**:
  - `Hearts`: 하트
  - `Diamonds`: 다이아몬드
  - `Clubs`: 클럽
  - `Spades`: 스페이드
- **deriving**:
  - `Show`: 문자열로 표현 가능 (출력)
  - `Eq`: 동등성 비교 가능 (==, /=)

### 1.2 Rank (숫자/값) 정의

```haskell
data Rank = Ace | Two | Three | Four | Five | Six | Seven
          | Eight | Nine | Ten | Jack | Queen | King
          deriving (Show, Eq, Enum)
```

- **값 생성자들**: Ace부터 King까지의 13가지 값
- **deriving**:
  - `Show`: 문자열 표현
  - `Eq`: 동등성 비교
  - `Enum`: 순서가 있는 값으로 취급
    ```haskell
    -- Enum의 예시 사용
    [Two .. Five]  -- [Two, Three, Four, Five]
    succ Two       -- Three
    pred King      -- Queen
    ```

### 1.3 Card (카드) 정의

```haskell
data Card = Card Rank Suit deriving (Show, Eq)
```

- **구조**: Rank와 Suit의 조합
- **사용 예시**:
  ```haskell
  aceOfHearts = Card Ace Hearts
  twoOfSpades = Card Two Spades
  ```

## 2. 게임 상태 관련 타입

### 2.1 Hand (손패) 타입 별칭

```haskell
type Hand = [Card]
```

- **의미**: Card의 리스트를 Hand로 부를 수 있음
- **사용 예시**:
  ```haskell
  playerCards :: Hand
  playerCards = [Card Ace Hearts, Card King Spades]
  ```

### 2.2 GameState (게임 상태) 정의

```haskell
data GameState = GameState {
    playerHand :: Hand,   -- 플레이어의 카드들
    dealerHand :: Hand,   -- 딜러의 카드들
    deck :: [Card]        -- 남은 덱
} deriving Show
```

- **레코드 구문 사용**:
  - 각 필드에 자동으로 접근자 함수 생성
  - 필드 이름을 통한 명확한 데이터 접근

## 3. 타입 사용 예시

### 3.1 카드 생성

```haskell
-- 개별 카드 생성
aceHearts = Card Ace Hearts
kingSpades = Card King Spades

-- 핸드 생성
exampleHand :: Hand
exampleHand = [aceHearts, kingSpades]
```

### 3.2 게임 상태 조작

```haskell
-- 게임 상태 생성
initialState = GameState {
    playerHand = [Card Ace Hearts, Card King Diamonds],
    dealerHand = [Card Queen Spades, Card Two Clubs],
    deck = remainingCards
}

-- 필드 접근
playerCards = playerHand initialState
dealerCards = dealerHand initialState

-- 상태 업데이트
addCardToPlayer :: Card -> GameState -> GameState
addCardToPlayer card state =
    state { playerHand = card : playerHand state }
```

## 4. 타입 시스템의 장점

### 4.1 타입 안전성

```haskell
-- 컴파일러가 잘못된 사용을 방지
invalidCard = Card Hearts Ace  -- 컴파일 에러!
invalidHand = [Hearts, Spades] -- 컴파일 에러!
```

### 4.2 의미적 명확성

```haskell
-- 타입이 의도를 명확히 표현
shuffleDeck :: [Card] -> IO [Card]
dealCards :: Int -> GameState -> (Hand, GameState)
```

## 5. 데이터 모델링의 특징

1. **계층적 구조**:

   - Card는 Rank와 Suit로 구성
   - GameState는 여러 Hand와 deck으로 구성

2. **불변성**:

   - 모든 데이터 구조가 불변
   - 상태 변경은 새로운 값 생성으로 처리

3. **타입 추상화**:
   - Hand는 [Card]의 의미있는 별칭
   - 코드의 의도를 더 명확하게 표현

이러한 타입 정의는 블랙잭 게임의 도메인을 명확하고 타입 안전하게 모델링하며, 코드의 유지보수성과 이해도를 높여줍니다.

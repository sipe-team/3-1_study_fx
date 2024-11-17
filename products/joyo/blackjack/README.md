# 블랙잭 게임 타입 구조 분석

## 1. 카드 관련 타입 정의

### 1.1 Suit (카드 무늬)
```haskell
data Suit = Spades | Hearts | Diamonds | Clubs 
    deriving (Show, Eq)
```
- **정의**: 카드의 네 가지 무늬를 표현
  - `Spades`: 스페이드
  - `Hearts`: 하트
  - `Diamonds`: 다이아몬드
  - `Clubs`: 클럽
- **deriving**:
  - `Show`: 무늬를 문자열로 표현 가능
  - `Eq`: 무늬 간 동등성 비교 가능

### 1.2 Rank (카드 숫자/값)
```haskell
data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King 
    deriving (Show, Eq, Enum)
```
- **정의**: 카드의 13가지 값을 표현
  - `Ace`부터 `King`까지의 모든 가능한 카드 값
- **deriving**:
  - `Show`: 값을 문자열로 표현
  - `Eq`: 값 간 비교 가능
  - `Enum`: 값들 간의 순서 정의 (순회 가능)
    ```haskell
    -- Enum 예시
    ghci> [Ace .. Five]  -- 순차적 나열 가능
    [Ace,Two,Three,Four,Five]
    ```

### 1.3 Card (카드)
```haskell
data Card = Card Rank Suit 
    deriving (Show, Eq)
```
- **정의**: 실제 카드를 표현하는 타입
  - `Rank`와 `Suit`의 조합
- **사용 예시**:
  ```haskell
  let aceOfSpades = Card Ace Spades
  let kingOfHearts = Card King Hearts
  ```

## 2. 게임 상태 관련 타입 정의

### 2.1 Hand (손패)
```haskell
type Hand = [Card]
```
- **정의**: 카드 리스트의 타입 별칭
- **특징**: 플레이어나 딜러가 가지고 있는 카드들의 모음
- **사용 예시**:
  ```haskell
  let playerCards = [Card Ace Hearts, Card King Spades] :: Hand
  ```

### 2.2 GameState (게임 상태)
```haskell
data GameState = GameState {
    playerHand :: Hand,     -- 플레이어의 현재 패
    dealerHand :: Hand,     -- 딜러의 현재 패
    deck :: [Card]          -- 남은 덱
} deriving Show
```
- **정의**: 게임의 전체 상태를 표현하는 레코드 타입
- **필드**:
  - `playerHand`: 플레이어의 현재 카드들
  - `dealerHand`: 딜러의 현재 카드들
  - `deck`: 아직 뽑지 않은 카드들
- **사용 예시**:
  ```haskell
  -- 게임 상태 생성
  let gameState = GameState {
      playerHand = [Card Ace Hearts, Card King Diamonds],
      dealerHand = [Card Queen Spades],
      deck = remainingCards
  }
  
  -- 상태 접근
  let playersCards = playerHand gameState
  let dealersCards = dealerHand gameState
  ```

## 3. 전체 구조의 특징

1. **타입 안전성**
   - 모든 게임 요소가 타입으로 명확하게 정의됨
   - 컴파일 시점에 많은 오류 방지 가능

2. **확장성**
   - 새로운 게임 규칙이나 상태 추가 용이
   - 기존 타입을 기반으로 새로운 기능 구현 가능

3. **가독성**
   - 레코드 구문을 사용하여 게임 상태를 명확하게 표현
   - 각 컴포넌트의 역할이 타입 시그니처로 명확히 드러남

4. **재사용성**
   - 정의된 타입들은 다른 카드 게임에서도 재사용 가능
   - 모듈화된 구조로 유지보수 용이
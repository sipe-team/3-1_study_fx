# Haskell 게임 초기화 함수 코드 분석

아래 코드는 블랙잭 게임의 초기 상태를 설정하는 함수의 구현을 분석합니다:

```haskell
initGame :: IO GameState
initGame = do
    shuffled <- shuffleDeck fullDeck
    let (playerCards, rest1) = splitAt 2 shuffled
        (dealerCards, rest2) = splitAt 2 rest1
    return $ GameState playerCards dealerCards rest2
```

## 1. 함수 시그니처 분석

```haskell
initGame :: IO GameState
```

- **출력**: IO 액션 내의 GameState
- **IO 사용 이유**: 카드 덱 섞기에 난수 생성이 필요하기 때문

## 2. 구현부 단계별 분석

### 2.1 덱 섞기

```haskell
shuffled <- shuffleDeck fullDeck
```

- `fullDeck`: 정렬된 전체 카드 덱
- `shuffleDeck`: 앞서 분석한 덱 섞기 함수
- `shuffled`: 무작위로 섞인 카드 리스트

### 2.2 카드 분배

```haskell
let (playerCards, rest1) = splitAt 2 shuffled
    (dealerCards, rest2) = splitAt 2 rest1
```

#### splitAt 2 shuffled

- **첫 번째 분배**:
  - `playerCards`: 처음 2장의 카드 (플레이어 카드)
  - `rest1`: 남은 카드들

#### splitAt 2 rest1

- **두 번째 분배**:
  - `dealerCards`: 다음 2장의 카드 (딜러 카드)
  - `rest2`: 덱에 남은 카드들

### 2.3 게임 상태 생성

```haskell
return $ GameState playerCards dealerCards rest2
```

- 새로운 GameState 레코드 생성
- 플레이어 카드, 딜러 카드, 남은 덱을 포함

## 3. 실행 과정 예시

```haskell
-- 초기 상태
fullDeck = [Card Ace Hearts, Card King Spades, ...]  -- 52장의 카드

-- 섞은 후
shuffled = [Card Ten Hearts, Card Two Diamonds, Card King Clubs, Card Ace Spades, ...]

-- 분배 후
playerCards = [Card Ten Hearts, Card Two Diamonds]
dealerCards = [Card King Clubs, Card Ace Spades]
rest2 = [나머지 48장의 카드...]

-- 최종 GameState
GameState
    { playerHand = [Card Ten Hearts, Card Two Diamonds]
    , dealerHand = [Card King Clubs, Card Ace Spades]
    , deck = [나머지 카드...]
    }
```

## 4. 함수의 특징

### 4.1 장점

1. **명확성**

   - 각 단계가 명확하게 구분됨
   - let 구문을 통한 중간 결과 바인딩

2. **안전성**

   - 타입 시스템을 통한 안전성 보장
   - 부수 효과가 IO 모나드로 격리

3. **유지보수성**
   - 코드가 순차적으로 구성됨
   - 각 단계가 독립적으로 수정 가능

### 4.2 사용 패턴

```haskell
main :: IO ()
main = do
    initialState <- initGame
    -- 게임 진행...
```

## 5. 관련 타입 및 함수

### 5.1 GameState 타입

```haskell
data GameState = GameState
    { playerHand :: Hand
    , dealerHand :: Hand
    , deck :: [Card]
    } deriving Show

type Hand = [Card]
```

### 5.2 도우미 함수들

```haskell
-- 카드 보여주기
showHand :: String -> Hand -> IO ()
showHand label hand = do
    putStrLn $ label ++ ": " ++ intercalate ", " (map show hand)
    putStrLn $ "Total: " ++ show (handValue hand)

-- 초기 상태 출력
displayInitialState :: GameState -> IO ()
displayInitialState gs = do
    showHand "Your hand" (playerHand gs)
    putStrLn "Dealer's up card: " ++ show (head (dealerHand gs))
```

## 6. 사용 예시

### 6.1 기본 게임 시작

```haskell
startNewGame :: IO ()
startNewGame = do
    gs <- initGame
    showHand "Your hand" (playerHand gs)
    showHand "Dealer's hand" (dealerHand gs)
    -- 게임 진행...
```

### 6.2 여러 라운드 처리

```haskell
playMultipleRounds :: Int -> IO ()
playMultipleRounds rounds = do
    gs <- initGame
    playRounds rounds gs
  where
    playRounds 0 _ = return ()
    playRounds n gs = do
        playRound gs
        newGs <- initGame
        playRounds (n-1) newGs
```

## 7. 가능한 개선사항

### 7.1 에러 처리 추가

```haskell
initGame :: IO (Either String GameState)
initGame = do
    shuffled <- shuffleDeck fullDeck
    if length shuffled < 4
        then return $ Left "Not enough cards"
        else do
            let (playerCards, rest1) = splitAt 2 shuffled
                (dealerCards, rest2) = splitAt 2 rest1
            return $ Right $ GameState playerCards dealerCards rest2
```

### 7.2 초기화 옵션 추가

```haskell
data InitOptions = InitOptions
    { numDecks :: Int
    , shuffleTimes :: Int
    }

initGameWithOptions :: InitOptions -> IO GameState
initGameWithOptions opts = do
    -- 구현...
```

이 함수는 블랙잭 게임의 기초를 설정하는 중요한 역할을 합니다. 게임의 공정성과 초기 상태 설정의 명확성을 보장합니다.

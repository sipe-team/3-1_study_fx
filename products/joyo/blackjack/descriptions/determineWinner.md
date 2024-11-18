# Haskell determineWinner 함수 코드 분석

아래 코드는 블랙잭 게임의 승자를 결정하고 결과를 출력하는 함수를 분석합니다:

```haskell
determineWinner :: GameState -> IO ()
determineWinner gs = do
    let playerScore = handValue (playerHand gs)
        dealerScore = handValue (dealerHand gs)
    putStrLn $ "\nFinal scores:"
    showHand "Your hand" (playerHand gs)
    showHand "Dealer's hand" (dealerHand gs)
    if playerScore > 21
        then putStrLn "You bust! Dealer wins!"
        else if dealerScore > 21
            then putStrLn "Dealer busts! You win!"
            else if playerScore > dealerScore
                then putStrLn "You win!"
                else if playerScore < dealerScore
                    then putStrLn "Dealer wins!"
                    else putStrLn "Push!"
```

## 1. 함수 시그니처 분석

```haskell
determineWinner :: GameState -> IO ()
```

- **입력**: 최종 게임 상태 (GameState)
- **출력**: IO () (출력만 수행, 값 반환 없음)
- **목적**: 게임 결과 판정 및 출력

## 2. 로직 구조 분석

### 2.1 점수 계산

```haskell
let playerScore = handValue (playerHand gs)
    dealerScore = handValue (dealerHand gs)
```

- let 구문으로 지역 변수 정의
- 각 플레이어의 최종 점수 계산

### 2.2 결과 표시

```haskell
putStrLn $ "\nFinal scores:"
showHand "Your hand" (playerHand gs)
showHand "Dealer's hand" (dealerHand gs)
```

- 최종 상태 출력
- 양쪽의 패와 점수 표시

### 2.3 승자 판정 로직

```haskell
if playerScore > 21               -- 플레이어 버스트
    then ...
    else if dealerScore > 21      -- 딜러 버스트
        then ...
        else if playerScore > dealerScore  -- 플레이어 승
            then ...
            else if playerScore < dealerScore  -- 딜러 승
                then ...
                else ...          -- 무승부
```

## 3. 승리 조건 분석

### 3.1 우선순위 순서

1. 플레이어 버스트 체크
2. 딜러 버스트 체크
3. 점수 비교
4. 무승부 확인

### 3.2 구체적 조건

```haskell
-- 패배 조건
playerScore > 21     -- 플레이어 버스트

-- 승리 조건
dealerScore > 21     -- 딜러 버스트
playerScore > dealerScore  -- 높은 점수

-- 무승부 조건
playerScore == dealerScore  -- 같은 점수
```

## 4. 실행 시나리오

### 4.1 플레이어 버스트

```
Final scores:
Your hand: [10, 5, 8] (23)
Dealer's hand: [10, 6] (16)
You bust! Dealer wins!
```

### 4.2 딜러 버스트

```
Final scores:
Your hand: [10, 8] (18)
Dealer's hand: [10, 5, 8] (23)
Dealer busts! You win!
```

### 4.3 일반 승부

```
Final scores:
Your hand: [10, 9] (19)
Dealer's hand: [10, 8] (18)
You win!
```

### 4.4 무승부

```
Final scores:
Your hand: [10, 7] (17)
Dealer's hand: [10, 7] (17)
Push!
```

## 5. 구현 특징

### 5.1 중첩된 if-else 구조

```haskell
if condition1
    then action1
    else if condition2
        then action2
        else if condition3
            then action3
            else action4
```

- 명확한 우선순위
- 순차적 평가

### 5.2 상태 출력

```haskell
showHand "Your hand" (playerHand gs)
showHand "Dealer's hand" (dealerHand gs)
```

- 투명한 결과 표시
- 결정 근거 제공

## 6. 가능한 개선사항

### 6.1 패턴 매칭 사용

```haskell
determineWinner :: GameState -> IO ()
determineWinner gs = do
    let playerScore = handValue (playerHand gs)
        dealerScore = handValue (dealerHand gs)
    putStrLn "\nFinal scores:"
    showHand "Your hand" (playerHand gs)
    showHand "Dealer's hand" (dealerHand gs)
    case (playerScore > 21, dealerScore > 21) of
        (True, _)     -> putStrLn "You bust! Dealer wins!"
        (_, True)     -> putStrLn "Dealer busts! You win!"
        _             -> case compare playerScore dealerScore of
            GT -> putStrLn "You win!"
            LT -> putStrLn "Dealer wins!"
            EQ -> putStrLn "Push!"
```

### 6.2 승리 조건 모듈화

```haskell
data GameResult = PlayerWin | DealerWin | Push

getGameResult :: Int -> Int -> GameResult
getGameResult playerScore dealerScore
    | playerScore > 21 = DealerWin
    | dealerScore > 21 = PlayerWin
    | playerScore > dealerScore = PlayerWin
    | playerScore < dealerScore = DealerWin
    | otherwise = Push

showResult :: GameResult -> IO ()
showResult PlayerWin = putStrLn "You win!"
showResult DealerWin = putStrLn "Dealer wins!"
showResult Push = putStrLn "Push!"
```

## 7. 주요 특징

1. **명확한 규칙**

   - 블랙잭 규칙의 충실한 구현
   - 우선순위가 명확한 판정

2. **투명한 결과**

   - 모든 정보 표시
   - 판단 근거 제공

3. **단순성**
   - 직관적인 로직 구조
   - 이해하기 쉬운 구현

이 함수는 게임의 최종 결과를 결정하고 표시하는 중요한 역할을 수행하며, 블랙잭 규칙을 명확하게 구현하고 있습니다.

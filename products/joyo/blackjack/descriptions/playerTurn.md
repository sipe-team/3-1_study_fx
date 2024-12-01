# Haskell playerTurn 함수 코드 분석

아래 코드는 블랙잭 게임에서 플레이어의 턴을 처리하는 함수를 분석합니다:

```haskell
playerTurn :: GameState -> IO GameState
playerTurn gs = do
    showHand "Your hand" (playerHand gs)
    putStr "Hit or Stand? (h/s): "
    choice <- getLine
    case choice of
        "h" -> do
            let (card, newGs) = drawCard gs
            let newPlayerHand = card : playerHand gs
            if handValue newPlayerHand > 21
                then do
                    putStrLn "Bust!"
                    return newGs { playerHand = newPlayerHand }
                else playerTurn newGs { playerHand = newPlayerHand }
        "s" -> return gs
        _ -> playerTurn gs
```

## 1. 함수 시그니처 분석

```haskell
playerTurn :: GameState -> IO GameState
```

- **입력**: 현재 게임 상태 (GameState)
- **출력**: IO 액션으로 감싸진 새로운 게임 상태
- **IO 사용**: 사용자 입력과 출력이 필요하기 때문

## 2. 주요 구성 요소 분석

### 2.1 초기 상태 표시와 입력

```haskell
showHand "Your hand" (playerHand gs)  -- 현재 패 보여주기
putStr "Hit or Stand? (h/s): "        -- 선택 프롬프트
choice <- getLine                      -- 사용자 입력 받기
```

### 2.2 case 문을 통한 선택 처리

```haskell
case choice of
    "h" -> -- Hit 처리
    "s" -> -- Stand 처리
    _   -> -- 잘못된 입력 처리
```

### 2.3 Hit 선택시 처리

```haskell
"h" -> do
    let (card, newGs) = drawCard gs            -- 새 카드 뽑기
    let newPlayerHand = card : playerHand gs   -- 새 카드 추가
    if handValue newPlayerHand > 21            -- 버스트 체크
        then do
            putStrLn "Bust!"
            return newGs { playerHand = newPlayerHand }
        else playerTurn newGs { playerHand = newPlayerHand }
```

## 3. 실행 흐름 분석

### 3.1 기본 실행 순서

1. 현재 손패 표시
2. 사용자에게 선택 요청
3. 입력 처리:
   - "h" (Hit): 새 카드 뽑기
   - "s" (Stand): 현재 상태로 종료
   - 그 외: 다시 시도

### 3.2 Hit 선택시 세부 흐름

1. 카드 뽑기
2. 새 패 생성
3. 점수 확인
4. 결과에 따른 처리:
   - 버스트: 게임 종료
   - 21 이하: 다음 턴 계속

## 4. 주요 패턴과 기능

### 4.1 재귀 패턴

```haskell
-- 잘못된 입력시 재귀
_ -> playerTurn gs

-- Hit 후 계속 진행시 재귀
else playerTurn newGs { playerHand = newPlayerHand }
```

### 4.2 상태 갱신

```haskell
newGs { playerHand = newPlayerHand }
```

- 레코드 업데이트 구문 사용
- 불변 데이터로 새 상태 생성

## 5. 사용 예시

### 5.1 게임 루프에서의 사용

```haskell
playGame :: IO ()
playGame = do
    initialState <- initGame
    finalState <- playerTurn initialState
    -- 게임 계속...
```

### 5.2 실행 시나리오

```haskell
-- 시나리오 1: Hit and Stand
Initial hand: [10, 6]
> h
New hand: [10, 6, 3]
> s
Final hand: [10, 6, 3]

-- 시나리오 2: Bust
Initial hand: [10, 6]
> h
New hand: [10, 6, 8]
Bust!
```

## 6. 에러 처리와 안전성

### 6.1 입력 검증

- 잘못된 입력은 무시하고 재시도
- 명시적인 에러 메시지는 없음

### 6.2 상태 관리

- 모든 상태 변경이 명시적
- IO 모나드 내에서 안전하게 처리

## 7. 가능한 개선사항

### 7.1 더 나은 사용자 피드백

```haskell
playerTurn gs = do
    showHand "Your hand" (playerHand gs)
    putStr "Hit or Stand? (h/s): "
    choice <- getLine
    case choice of
        "h" -> -- 현재와 동일
        "s" -> do
            putStrLn "You stand."
            return gs
        _ -> do
            putStrLn "Invalid input. Please enter 'h' or 's'."
            playerTurn gs
```

### 7.2 게임 종료 조건 추가

```haskell
playerTurn gs = do
    showHand "Your hand" (playerHand gs)
    when (handValue (playerHand gs) == 21) $ do
        putStrLn "Blackjack!"
        return gs
    -- 나머지 로직
```

## 8. 주요 특징

1. **대화형 처리**

   - 사용자 입력에 따른 동적 진행
   - 명확한 피드백 제공

2. **재귀적 구조**

   - 자연스러운 게임 진행 표현
   - 상태 관리의 명확성

3. **타입 안전성**
   - IO 액션의 명시적 처리
   - 게임 상태의 일관성 유지

이 함수는 블랙잭 게임의 핵심 상호작용을 담당하며, 함수형 프로그래밍의 주요 개념들을 잘 활용하고 있습니다.

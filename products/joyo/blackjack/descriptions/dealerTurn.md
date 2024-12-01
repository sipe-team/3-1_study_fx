# Haskell dealerTurn 함수 코드 분석

아래 코드는 블랙잭 게임에서 딜러의 턴을 처리하는 함수를 분석합니다:

```haskell
dealerTurn :: GameState -> IO GameState
dealerTurn gs = do
    showHand "Dealer's hand" (dealerHand gs)
    if handValue (dealerHand gs) < 17
        then do
            let (card, newGs) = drawCard gs
            let newDealerHand = card : dealerHand gs
            putStrLn "Dealer hits"
            if handValue newDealerHand > 21
                then do
                    putStrLn "Dealer busts!"
                    return newGs { dealerHand = newDealerHand }
                else dealerTurn newGs { dealerHand = newDealerHand }
        else do
            putStrLn "Dealer stands"
            return gs
```

## 1. 함수 시그니처 분석

```haskell
dealerTurn :: GameState -> IO GameState
```

- **입력**: 현재 게임 상태 (GameState)
- **출력**: IO 액션으로 감싸진 새로운 게임 상태
- **특징**: 플레이어와 달리 사용자 입력 없이 자동 진행

## 2. 주요 로직 분석

### 2.1 딜러 규칙

```haskell
if handValue (dealerHand gs) < 17
```

- 딜러는 핸드 값이 17 미만이면 반드시 히트
- 17 이상이면 반드시 스탠드
- 블랙잭의 기본 규칙을 구현

### 2.2 카드 뽑기 처리

```haskell
let (card, newGs) = drawCard gs
let newDealerHand = card : dealerHand gs
```

- 새 카드를 뽑아 현재 패에 추가
- cons 연산자(`:`)로 리스트 앞에 새 카드 추가

### 2.3 버스트 체크

```haskell
if handValue newDealerHand > 21
    then do
        putStrLn "Dealer busts!"
        return newGs { dealerHand = newDealerHand }
    else dealerTurn newGs { dealerHand = newDealerHand }
```

- 21을 초과하면 버스트로 턴 종료
- 21 이하면 재귀적으로 계속 진행

## 3. 실행 흐름 분석

### 3.1 기본 플로우

```
1. 현재 패 보여주기
2. 패 값 확인 (17 기준)
3A. 17 미만:
    - 카드 뽑기
    - 버스트 체크
    - 계속 또는 종료
3B. 17 이상:
    - 스탠드
    - 종료
```

### 3.2 상태 변이 패턴

```haskell
-- 새 상태 생성
newGs { dealerHand = newDealerHand }
```

- 불변 데이터 구조 사용
- 레코드 업데이트 구문으로 새 상태 생성

## 4. 실행 예시

### 4.1 17 미만 시나리오

```
Dealer's hand: [10, 5]  (합계: 15)
Dealer hits
새 카드: 4
새 합계: 19
Dealer stands
```

### 4.2 버스트 시나리오

```
Dealer's hand: [10, 8]  (합계: 18)
Dealer hits
새 카드: 6
새 합계: 24
Dealer busts!
```

## 5. 플레이어턴과의 차이점

1. **자동화된 결정**

   - 사용자 입력 없음
   - 규칙 기반 자동 진행

2. **단순한 로직**
   - 17을 기준으로 한 이진 결정
   - 추가적인 전략 없음

## 6. 구현 특징

### 6.1 재귀적 구조

```haskell
-- 조건부 재귀
else dealerTurn newGs { dealerHand = newDealerHand }
```

- 17 미만일 때 계속 진행
- 종료 조건이 명확함 (버스트 또는 17 이상)

### 6.2 상태 관리

```haskell
return newGs { dealerHand = newDealerHand }
```

- 명시적 상태 갱신
- 불변성 유지

## 7. 가능한 개선사항

### 7.1 더 자세한 정보 표시

```haskell
dealerTurn gs = do
    showHand "Dealer's hand" (dealerHand gs)
    let currentValue = handValue (dealerHand gs)
    putStrLn $ "Current value: " ++ show currentValue
    -- 나머지 로직
```

### 7.2 에러 처리 추가

```haskell
dealerTurn gs = do
    case deck gs of
        [] -> do
            putStrLn "No cards left in deck!"
            return gs
        _  -> do
            -- 현재 로직
```

### 7.3 딜러 전략 커스터마이즈

```haskell
data DealerStrategy = Strategy { hitLimit :: Int }

dealerTurn :: DealerStrategy -> GameState -> IO GameState
dealerTurn strategy gs = do
    if handValue (dealerHand gs) < hitLimit strategy
        then do
            -- 히트 로직
        else do
            -- 스탠드 로직
```

## 8. 주요 특징

1. **결정론적 동작**

   - 명확한 규칙 기반
   - 예측 가능한 실행

2. **상태 관리**

   - IO 모나드 내 안전한 상태 변이
   - 불변 데이터 구조 활용

3. **가독성**
   - 명확한 조건문 구조
   - 직관적인 게임 로직 표현

이 함수는 블랙잭의 딜러 규칙을 충실히 구현하면서, Haskell의 함수형 프로그래밍 패턴을 잘 활용하고 있습니다.

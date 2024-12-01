# Haskell showHand 함수 코드 분석

아래 코드는 플레이어나 딜러의 카드 패를 출력하는 함수의 구현을 분석합니다:

```haskell
showHand :: String -> Hand -> IO ()
showHand label hand = do
    putStrLn $ label ++ ": " ++ intercalate ", " (map show hand)
    putStrLn $ "Total: " ++ show (handValue hand)
```

## 1. 함수 시그니처 분석

```haskell
showHand :: String -> Hand -> IO ()
```

- **입력 1**: String (라벨, 예: "Your hand" 또는 "Dealer's hand")
- **입력 2**: Hand (카드 리스트, type Hand = [Card])
- **출력**: IO () (출력 동작을 수행하는 IO 액션)

## 2. 문법적 요소 분석

### 2.1 do 표기법

```haskell
do
    action1
    action2
```

- 여러 IO 액션을 순차적으로 실행
- 각 줄은 IO 액션을 나타냄

### 2.2 문자열 연결

```haskell
label ++ ": " ++ intercalate ", " (map show hand)
```

- `++`: 문자열 연결 연산자
- `intercalate`: Data.List의 함수, 리스트 요소들을 구분자로 연결

### 2.3 함수 합성

```haskell
map show hand  -- 각 카드를 문자열로 변환
```

- `map`: 리스트의 각 요소에 함수 적용
- `show`: 값을 String으로 변환

## 3. 실행 예시

### 3.1 입력 예시

```haskell
label = "Your hand"
hand = [Card Ace Hearts, Card King Diamonds]
```

### 3.2 출력 결과

```
Your hand: Card Ace Hearts, Card King Diamonds
Total: 21
```

## 4. 세부 동작 분석

### 4.1 첫 번째 줄 출력

```haskell
putStrLn $ label ++ ": " ++ intercalate ", " (map show hand)
```

단계별 실행:

1. `map show hand`

   - 각 카드를 문자열로 변환
   - 결과: `["Card Ace Hearts", "Card King Diamonds"]`

2. `intercalate ", "`

   - 카드 문자열들을 쉼표와 공백으로 연결
   - 결과: `"Card Ace Hearts, Card King Diamonds"`

3. `label ++ ": " ++`
   - 라벨과 구분자 추가
   - 결과: `"Your hand: Card Ace Hearts, Card King Diamonds"`

### 4.2 두 번째 줄 출력

```haskell
putStrLn $ "Total: " ++ show (handValue hand)
```

- `handValue hand`: 패의 총 점수 계산
- `show`: 점수를 문자열로 변환
- 결과: `"Total: 21"`

## 5. 사용 예시

### 5.1 기본 사용

```haskell
main :: IO ()
main = do
    let playerHand = [Card Ace Hearts, Card King Diamonds]
    showHand "Your hand" playerHand
```

### 5.2 게임 상태에서 사용

```haskell
displayGameState :: GameState -> IO ()
displayGameState gs = do
    showHand "Your hand" (playerHand gs)
    showHand "Dealer's hand" (dealerHand gs)
```

## 6. 관련 함수들

### 6.1 handValue 함수 (관련)

```haskell
handValue :: Hand -> Int
handValue hand =
    let total = sum $ map cardValue hand
        aces = length $ filter isAce hand
    in if total > 21 && aces > 0
       then total - (10 * aces)
       else total
```

### 6.2 카드 표시 변형

```haskell
-- 딜러의 첫 카드만 보여주기
showDealerInitialHand :: Hand -> IO ()
showDealerInitialHand hand = do
    let firstCard = head hand
    putStrLn $ "Dealer's hand: " ++ show firstCard ++ ", <hidden>"
```

## 7. 개선 가능한 부분

### 7.1 에러 처리 추가

```haskell
showHand :: String -> Hand -> IO ()
showHand label [] = putStrLn $ label ++ ": Empty hand"
showHand label hand = do
    putStrLn $ label ++ ": " ++ intercalate ", " (map show hand)
    putStrLn $ "Total: " ++ show (handValue hand)
```

### 7.2 출력 포맷팅 개선

```haskell
showHand :: String -> Hand -> IO ()
showHand label hand = do
    putStrLn $ replicate 40 '-'
    putStrLn $ label ++ ":"
    putStrLn $ intercalate ", " (map show hand)
    putStrLn $ "Total: " ++ show (handValue hand)
    putStrLn $ replicate 40 '-'
```

## 8. 주요 특징

1. **가독성**

   - 명확한 출력 형식
   - 직관적인 정보 표시

2. **모듈성**

   - 독립적인 출력 함수
   - 재사용 가능한 구조

3. **IO 처리**
   - 순수하지 않은 출력 작업을 IO 모나드로 캡슐화
   - do 표기법으로 순차적 실행 명시

이 함수는 게임의 현재 상태를 사용자에게 보여주는 중요한 인터페이스 역할을 합니다.

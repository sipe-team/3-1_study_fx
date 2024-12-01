# handValue 함수 분석

## 1. 함수 시그니처와 코드

```haskell
handValue :: Hand -> Int
handValue hand =
    let total = sum $ map cardValue hand
        aces = length $ filter (\(Card rank _) -> rank == Ace) hand
    in if total > 21 && aces > 0
        then total - (10 * aces)
        else total
```

## 2. 함수 분석

### 2.1 구성 요소

1. **총합 계산**

   ```haskell
   total = sum $ map cardValue hand
   ```

   - 모든 카드의 값을 계산하고 합산

2. **에이스 카운팅**

   ```haskell
   aces = length $ filter (\(Card rank _) -> rank == Ace) hand
   ```

   - 람다 함수로 Ace 카드만 필터링
   - 에이스 개수 계산

3. **버스트 처리**
   ```haskell
   if total > 21 && aces > 0
       then total - (10 * aces)
       else total
   ```
   - 21 초과시 에이스 값 조정
   - 에이스당 10 감소 (11 → 1)

### 2.2 구현 특징

1. **에이스 유연성**: 상황에 따라 1 또는 11로 처리
2. **최적 점수 계산**: 버스트를 피하면서 최대 점수 도출
3. **복잡도**: O(n) where n is hand size

## 3. 사용 예시

```haskell
-- 기본 사용
handValue [Card Ace Hearts, Card King Spades]  -- 21
handValue [Card Ace Hearts, Card Ace Spades]   -- 12 (11+1)
handValue [Card Ten Hearts, Card King Spades]  -- 20
```

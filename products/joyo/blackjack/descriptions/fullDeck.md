# fullDeck 함수 분석

## 1. 함수 시그니처와 코드

```haskell
fullDeck :: [Card]
fullDeck = [Card rank suit | suit <- [Hearts, Diamonds, Clubs, Spades],
                            rank <- [Ace .. King]]
```

## 2. 함수 분석

### 2.1 리스트 컴프리헨션

- 두 개의 제너레이터 사용:
  1. `suit <- [Hearts, Diamonds, Clubs, Spades]`
  2. `rank <- [Ace .. King]`

### 2.2 동작 원리

1. **외부 순회**: 각 무늬(suit)에 대해
2. **내부 순회**: 각 숫자(rank)에 대해
3. **카드 생성**: 모든 가능한 조합으로 Card 생성

### 2.3 특징

1. **완전성**: 52장의 모든 카드 생성
2. **순서**: 무늬 별로 그룹화됨
3. **Enum 활용**: `[Ace .. King]`으로 순차 생성

## 3. 생성되는 덱의 구조

```haskell
-- 생성되는 카드들의 순서:
[
    -- Hearts
    Card Ace Hearts,
    Card Two Hearts,
    ...,
    Card King Hearts,

    -- Diamonds
    Card Ace Diamonds,
    ...,

    -- (Clubs, Spades도 동일한 패턴)
]
```

## 4. 사용 예시

```haskell
-- 전체 덱 생성
let deck = fullDeck

-- 특정 무늬의 카드만 필터링
let hearts = filter (\(Card _ suit) -> suit == Hearts) fullDeck

-- 특정 숫자의 카드만 필터링
let aces = filter (\(Card rank _) -> rank == Ace) fullDeck
```

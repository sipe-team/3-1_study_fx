# cardValue 함수 분석

## 1. 함수 시그니처와 코드

```haskell
cardValue :: Card -> Int
cardValue (Card rank _) = case rank of
    Ace -> 11
    King -> 10
    Queen -> 10
    Jack -> 10
    _ -> fromEnum rank + 1
```

## 2. 함수 분석

### 2.1 패턴 매칭

- `(Card rank _)`: Card의 rank만 사용하고 suit는 무시
- `_`: 와일드카드로 suit 값을 무시

### 2.2 case 문 분석

- 특별한 카드들의 값 처리:
  - Ace: 11
  - King, Queen, Jack: 10
- 나머지 카드들:
  - `fromEnum rank + 1`: Enum 인스턴스를 활용
  - Two는 2, Three는 3 등의 값 반환

### 2.3 구현 특징

1. **단순성**: 직관적인 값 매핑
2. **완전성**: 모든 가능한 rank에 대한 처리
3. **효율성**: O(1) 시간 복잡도

## 3. 사용 예시

```haskell
-- 기본 사용
cardValue (Card Ace Hearts)    -- 11
cardValue (Card King Spades)   -- 10
cardValue (Card Two Diamonds)  -- 2

-- 실제 게임에서의 활용
map cardValue [Card Ace Hearts, Card King Spades]  -- [11, 10]
```

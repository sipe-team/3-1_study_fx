# Haskell의 List Comprehension과 <- 연산자

## 1. List Comprehension 기본 개념

List Comprehension은 수학의 집합 표기법을 프로그래밍으로 구현한 것입니다.

### 1.1 기본 문법
```haskell
[표현식 | 제너레이터, 조건...]
```
- **표현식**: 결과로 만들어질 각 요소의 형태
- **제너레이터**: `<-` 를 사용하여 리스트에서 값을 하나씩 추출
- **조건**: 선택적으로 사용할 수 있는 필터

## 2. <- (제너레이터) 연산자

### 2.1 기본 사용법
```haskell
-- x는 [1,2,3]의 각 요소를 순서대로 가리킴
[x * 2 | x <- [1,2,3]]
-- 결과: [2,4,6]
```

### 2.2 fullDeck 예시 분석
```haskell
fullDeck = [Card rank suit | suit <- [Hearts, Diamonds, Clubs, Spades],
                            rank <- [Ace .. King]]
```
이 코드는 다음과 같이 동작합니다:
1. `suit`가 `[Hearts, Diamonds, Clubs, Spades]`의 각 값을 순회
2. 각 `suit`에 대해 `rank`가 `[Ace .. King]`의 각 값을 순회
3. 각 조합에 대해 `Card rank suit`를 생성

이는 다음의 중첩 루프와 동일:
```haskell
-- 명시적인 재귀 버전
fullDeck = concat [
    [Card rank suit | rank <- [Ace .. King]] 
    | suit <- [Hearts, Diamonds, Clubs, Spades]
    ]

-- do 표기법 버전
fullDeck = do
    suit <- [Hearts, Diamonds, Clubs, Spades]
    rank <- [Ace .. King]
    return (Card rank suit)
```

## 3. 다양한 List Comprehension 예시

### 3.1 단순 매핑
```haskell
-- 숫자 리스트의 각 요소를 두 배로
[x * 2 | x <- [1..5]]
-- 결과: [2,4,6,8,10]
```

### 3.2 필터링 추가
```haskell
-- 짝수만 선택
[x | x <- [1..10], even x]
-- 결과: [2,4,6,8,10]
```

### 3.3 다중 제너레이터
```haskell
-- 두 리스트의 모든 조합
[(x,y) | x <- [1,2], y <- ['a','b']]
-- 결과: [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
```

### 3.4 중첩된 리스트 처리
```haskell
-- 2차원 리스트 평탄화
[ x | xs <- [[1,2],[3,4]], x <- xs]
-- 결과: [1,2,3,4]
```

## 4. 실용적인 예시들

### 4.1 카드 덱 변형
```haskell
-- 특정 무늬의 카드만 생성
spadesOnly = [Card rank Spades | rank <- [Ace .. King]]

-- 페이스 카드만 생성
faceCards = [Card rank suit | 
    suit <- [Hearts, Diamonds, Clubs, Spades],
    rank <- [Jack, Queen, King]]

-- 특정 값 이상의 카드만 생성
highCards = [Card rank suit | 
    suit <- [Hearts, Diamonds, Clubs, Spades],
    rank <- [Ten .. King]]
```

### 4.2 조건부 덱 생성
```haskell
-- 빨간 카드만 (하트와 다이아몬드)
redCards = [Card rank suit | 
    suit <- [Hearts, Diamonds],
    rank <- [Ace .. King]]

-- 숫자 카드만 (Ace와 페이스 카드 제외)
numberCards = [Card rank suit | 
    suit <- [Hearts, Diamonds, Clubs, Spades],
    rank <- [Two .. Ten]]
```

## 5. 주의사항과 팁

### 5.1 성능 고려사항
- List Comprehension은 새로운 리스트를 생성
- 큰 데이터셋의 경우 메모리 사용량 고려 필요

### 5.2 가독성
```haskell
-- 복잡한 조건은 별도 함수로 분리하는 것이 좋음
isValidCard card = ...

validCards = [card | card <- allCards, isValidCard card]
```

### 5.3 디버깅
```haskell
-- 중간 결과 확인을 위한 print 문 추가
debug = [trace ("Processing: " ++ show x) x | x <- [1..3]]
```

이러한 List Comprehension은 Haskell에서 리스트를 다룰 때 매우 강력하고 표현력 있는 도구입니다.
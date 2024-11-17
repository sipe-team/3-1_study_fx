# Haskell 덱 섞기 함수 코드 분석

아래 코드는 카드 덱을 무작위로 섞는 함수의 구현을 분석합니다:
```haskell
shuffleDeck :: [Card] -> IO [Card]
shuffleDeck deck = do
    gen <- newStdGen
    return $ map snd $ sortBy (\(a,_) (b,_) -> compare a b) 
           $ zip (randoms gen :: [Float]) deck
```

## 1. 함수 시그니처 분석
```haskell
shuffleDeck :: [Card] -> IO [Card]
```
- **입력**: 카드 리스트 `[Card]`
- **출력**: IO 액션 내의 카드 리스트 `IO [Card]`
- **IO 사용 이유**: 난수 생성이라는 부수 효과를 다루기 위함

## 2. 구현부 단계별 분석

### 2.1 do 표기법과 난수 생성기
```haskell
do
    gen <- newStdGen
```
- `do`: IO 액션들을 순차적으로 실행하기 위한 표기법
- `newStdGen`: 새로운 난수 생성기를 생성하는 IO 액션
- `gen`: 생성된 난수 생성기를 바인딩

### 2.2 함수 합성 체인 분석
```haskell
return $ map snd $ sortBy (\(a,_) (b,_) -> compare a b) $ zip (randoms gen :: [Float]) deck
```
실행 순서대로 분해:

1. **난수 리스트 생성**
```haskell
randoms gen :: [Float]
```
- 0과 1 사이의 Float 난수들의 무한 리스트 생성

2. **카드와 난수 매핑**
```haskell
zip (randoms gen :: [Float]) deck
```
- 결과 예시: `[(0.23, Card Ace Hearts), (0.87, Card King Spades), ...]`

3. **난수 기준 정렬**
```haskell
sortBy (\(a,_) (b,_) -> compare a b)
```
- 첫 번째 요소(난수)를 기준으로 정렬
- `_`는 패턴 매칭에서 무시되는 값

4. **카드만 추출**
```haskell
map snd
```
- 정렬된 튜플에서 두 번째 요소(카드)만 추출

5. **IO 모나드로 래핑**
```haskell
return
```
- 순수 값을 IO 모나드로 감싸기

## 3. 실행 과정 예시

입력 덱이 `[Card Ace Hearts, Card King Spades, Card Two Diamonds]`인 경우:

```haskell
-- 1. 난수 생성 후 zip
[(0.23, Card Ace Hearts), 
 (0.87, Card King Spades), 
 (0.45, Card Two Diamonds)]

-- 2. 난수로 정렬 후
[(0.23, Card Ace Hearts), 
 (0.45, Card Two Diamonds), 
 (0.87, Card King Spades)]

-- 3. 카드만 추출한 최종 결과
[Card Ace Hearts, Card Two Diamonds, Card King Spades]
```

## 4. 함수의 특징

### 4.1 장점
1. **공정성**
   - 모든 카드가 동일한 확률로 섞임
   - 암시적 편향 없음

2. **효율성**
   - 시간 복잡도: O(n log n)
   - 공간 복잡도: O(n)

3. **순수성 유지**
   - 난수 생성은 IO 모나드 내에서만 발생
   - 부수 효과가 타입 시스템에 명시적으
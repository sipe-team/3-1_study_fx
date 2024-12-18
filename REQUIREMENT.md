# 21점 카드 게임 요구사항

21점 카드게임은 블랙잭 게임을 간소화한 버전입니다.

## 게임 설정

### 플레이어

- 1명의 플레이어 vs. 딜러(컴퓨터).

### 덱

- 표준 52장 덱 사용(조커 제외).

### 카드 값

- 숫자 카드 (2–10): 카드의 숫자와 동일한 값.
- 얼굴 카드 (잭, 퀸, 킹): 각 10점.
- 에이스: 1점 또는 11점 (플레이어에게 유리한 값을 선택).

### 셔플

- 게임 시작 전 카드 섞기.

## 게임 진행 방식

### 초기 배분

- 플레이어와 딜러는 각각 2장의 카드를 받음.
- 플레이어의 카드는 모두 오픈, 딜러의 카드는 한 장만 공개.

### 플레이어의 턴

- 플레이어는 "히트" (카드를 더 받기) 또는 "스탠드" (카드를 그만 받기) 중 선택 가능.
- 플레이어는 딜러보다 카드를 먼저 받음.
- 플레이어의 총 점수가 21점을 초과하면 "버스트" 되어 즉시 패배.

### 딜러의 턴

- 딜러는 총 점수가 17점 이상이 될 때까지 카드를 받아야 함.
- 딜러가 21점을 초과하면 플레이어 즉시 승리.

## 승리 조건

### 블랙잭

- 플레이어의 초기 두 장 카드 합이 21점(에이스와 10점 카드)이면, 자동 승리. 단, 딜러도 블랙잭인 경우 무승부.

### 핸드 비교

플레이어와 딜러가 모두 버스트되지 않았을 경우 점수를 비교.

- 높은 점수를 가진 쪽이 승리.
- 점수가 같으면 무승부.

## 사용자 입력

- 게임 시작: 플레이어는 새로운 라운드를 시작할 수 있음.
- 히트 또는 스탠드: 플레이어는 "hit" 또는 "stand" 명령어로 카드를 받거나 멈출 수 있음.
- 게임 종료: 플레이어는 언제든지 게임을 종료할 수 있어야 함.

## 게임 인터페이스

### 화면 표시

- 플레이어의 핸드, 총 점수, 딜러의 공개된 카드가 각 턴 후에 표시됨.
- 각 라운드 종료 후 결과(승리, 패배, 무승부)를 보여줌.

### 사용자 친화적인 콘솔 프롬프트

- 각 단계에서 명확한 사용자 입력 안내 제공.

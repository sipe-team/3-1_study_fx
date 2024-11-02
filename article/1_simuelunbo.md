# Haskell 개발 환경 설정 (macOS)

Haskell로 개발을 시작하기 위해서는 몇 가지 핵심 도구들을 설치 (macOS 기준으로 Haskell 개발 환경을 구축하는 방법을 설명

## 1. GHCup 설치

### GHCup이란?

GHCup은 Haskell 도구 체인 관리자 즉 개발 환경 자체를 관리
Node.js의 nvm이나 Ruby의 rbenv와 비슷한 역할
Cabal, Stack 등 Haskell 개발에 필요한 핵심 도구들을 설치하고 버전을 관리할 수 있게 해줌

### 설치 방법

GHCup을 설치하는 방법에는 두 가지가 있습니다:

#### 1) curl을 이용한 설치 (권장)

터미널에서 다음 명령어를 실행

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

#### 2) Homebrew를 이용한 설치

```bash
brew install ghcup
```


## 2. Stack 설치

### Stack이란?

Stack은 Haskell 프로젝트의 dependency 관리와 빌드를 담당하는 도구
Node.js의 npm이나 java,kotlin에서 gradle, maven 그리고 Python의 pip와 유사한 역할을 함 
Stack의 주요 특징:

- 프로젝트별로 독립된 GHC 버전 관리
- 의존성 자동 관리
- 프로젝트 템플릿 제공

### 설치 방법

#### 1) GHCup을 통한 설치 (권장)

```bash
ghcup install stack
```

#### 2) curl을 이용한 직접 설치

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

#### 3) Homebrew를 이용한 설치

```bash
brew install haskell-stack
```

## 3. HLS (Haskell Language Server) 설치

### HLS란?

Haskell Language Server는 IDE 기능을 제공하는 언어 서버입니다. 다음과 같은 기능들을 제공합니다:

- 코드 자동 완성
- 실시간 오류 검사
- 코드 네비게이션
- 리팩토링 도구
- 타입 정보 표시
- 문서화 지원

### 설치 방법

#### 1) GHCup을 통한 설치 (권장)

```bash
ghcup install hls
```

#### 2) Homebrew를 통한 설치

```bash
brew install haskell-language-server
```

## 4. 첫 Haskell 코드 실행하기

### GHCi 시작하기
GHCi는 Haskell의 대화형 인터프리터입니다. 터미널에서 다음 명령어로 실행 가능:

```bash
ghci
```

```haskell
Prelude> 2 + 2
4

Prelude> "Hello, " ++ "Haskell!"
"Hello, Haskell!"
```

### GHCi 종료하기
```haskell
Prelude> :q
```

## 5. Haskell 프로젝트 시작하기

### 새 프로젝트 생성하기
Stack을 사용하여 새 프로젝트 생성 가능:

```bash
stack new blackjack
```

### 프로젝트 구조 이해하기
생성된 프로젝트는 다음과 같은 구조를 가집니다:

```
blackjack/
├── app/                  # 실행 파일 관련 코드
│   └── Main.hs          # 프로그램의 진입점
├── src/                  # 라이브러리 소스 코드
│   └── Lib.hs           # 주요 기능 구현
├── test/                 # 테스트 코드
├── package.yaml         # 프로젝트 설정 및 의존성 정의
├── stack.yaml           # Stack 도구 설정
└── blackjack.cabal    # package.yaml에서 자동 생성되는 파일
```

각 파일과 디렉토리의 역할:
- `app/Main.hs`: 프로그램의 시작점, 실행 파일의 메인 로직
- `src/Lib.hs`: 프로젝트의 핵심 기능을 구현하는 코드가 위치
- `test/`: 단위 테스트와 통합 테스트 코드가 포함됨
- `package.yaml`: 프로젝트의 메타데이터와 의존성을 정의
- `stack.yaml`: 빌드 도구 설정과 프로젝트에서 사용할 GHC 버전을 지정

## 6. 프로젝트 빌드 및 실행하기

### 프로젝트 빌드

```bash
# 프로젝트에 필요한 GHC 버전 설치
stack setup

# 프로젝트 빌드
stack build
```

### 프로젝트 실행

```bash
# 프로젝트 실행
stack run

# 테스트 실행
stack test
```

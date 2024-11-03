# Haskell 개발 환경 설정 (macOS)

Haskell로 개발을 시작하기 위해서는 몇 가지 핵심 도구들을 설치해야 합니다. 이 가이드에서는 macOS에서 Haskell 개발 환경을 구축하는 방법을 설명합니다.

## 1. GHCup 설치

### GHCup이란?

GHCup(지에이치컵 또는 지에이치씨업으로 발음)은 Haskell 도구체인 관리자입니다. Node.js의 nvm이나 Ruby의 rbenv와 비슷한 역할을 합니다. GHC(Glasgow Haskell Compiler), Cabal, Stack 등 Haskell 개발에 필요한 핵심 도구들을 설치하고 버전을 관리할 수 있게 해줍니다.

### 설치 방법

GHCup을 설치하는 방법에는 두 가지가 있습니다:

#### 1) curl을 이용한 설치 (권장)

터미널에서 다음 명령어를 실행합니다:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

#### 2) Homebrew를 이용한 설치

```bash
brew install ghcup
```

설치 과정에서 몇 가지 선택사항이 나타날 것입니다. 기본값을 추천드립니다.

## 2. Stack 설치

### Stack이란?

Stack은 Haskell 프로젝트의 의존성 관리와 빌드를 담당하는 도구입니다. Node.js의 npm이나 Python의 pip와 유사한 역할을 합니다. Stack의 주요 특징은:

- 프로젝트별로 독립된 GHC 버전 관리
- 재현 가능한 빌드
- 의존성 자동 관리
- 프로젝트 템플릿 제공

### 설치 방법

Stack을 설치하는 방법에는 세 가지가 있습니다:

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

HLS를 설치하는 방법에는 두 가지가 있습니다:

#### 1) GHCup을 통한 설치 (권장)

```bash
ghcup install hls
```

#### 2) Homebrew를 통한 설치

```bash
brew install haskell-language-server
```

## 설치 확인

각 도구가 정상적으로 설치되었는지 확인하려면 다음 명령어들을 실행해보세요:

```bash
ghcup list    # 설치된 도구 확인
혹은
ghcup tui    # 인터렉티브한 설치 도구 확인
```

## IDE 설정

VSCode를 사용하시는 경우, "Haskell" 확장을 설치하시면 HLS의 모든 기능을 활용할 수 있습니다.

## 주의사항

- macOS에서 개발 도구를 설치하기 전에 Xcode Command Line Tools가 설치되어 있어야 합니다:

```bash
xcode-select --install
```

- Homebrew를 통해 필요한 시스템 의존성을 설치할 수 있습니다:

```bash
brew install libffi
```

## 설치 방법 선택 가이드

각 설치 방법의 장단점을 비교해보겠습니다:

### GHCup을 통한 설치

- 장점:
  - 버전 관리가 용이
  - 다른 Haskell 도구들과의 호환성이 보장됨
  - 업그레이드와 다운그레이드가 쉬움
- 단점:
  - 초기 설정에 시간이 좀 더 걸릴 수 있음

### Homebrew를 통한 설치

- 장점:
  - 설치가 간단함
  - 다른 macOS 패키지들과 함께 관리 가능
- 단점:
  - 버전 관리가 덜 유연함
  - 최신 버전이 늦게 업데이트될 수 있음

### curl을 통한 직접 설치

- 장점:
  - 별도의 패키지 관리자 없이 설치 가능
  - 최신 버전을 바로 받을 수 있음
- 단점:
  - 버전 관리가 수동으로 이루어져야 함
  - 제거나 업데이트가 번거로울 수 있음

초보자의 경우 GHCup을 통한 설치를 권장드립니다. 이는 가장 표준적이고 관리하기 쉬운 방법입니다.

이제 Haskell 개발을 시작할 준비가 되었습니다! 🎉

## 첫 Haskell 코드 실행하기

Haskell 인터프리터(GHCi)를 사용하면 터미널에서 바로 Haskell 코드를 실행해볼 수 있습니다.

### GHCi 시작하기
터미널에서 다음 명령어로 인터프리터를 실행합니다:

- 특정 ghci 버전이 설치된 경우 버전까지 명시해 줘야 합니다.
- 혹은 ghci 와 aliasing 을 설정해 주세요.
- 설치된 ghci 는 ~./ghcup/bin 에서 확인할 수 있습니다.

```bash
ghci
```

### 간단한 코드 예제
GHCi 프롬프트에서 다음과 같은 간단한 코드를 입력해보세요:
```haskell
Prelude> 2 + 2
4

Prelude> "Hello, " ++ "Haskell!"
"Hello, Haskell!"

Prelude> let double x = x * 2
Prelude> double 21
42
```

### GHCi 종료하기
인터프리터를 종료하려면 다음 명령어를 입력하거나:
```haskell
Prelude> :quit
```
`Ctrl + D`를 누르면 됩니다.

이제 Haskell 개발 맛보기도 성공하였습니다! 🎉
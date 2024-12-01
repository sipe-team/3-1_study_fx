# 현대적인 Effect 다루기 - ZIO

함수형 패러타임에서 Effect를 다루기위해 다양한 방법들이 제시되어 왔다. 이들은 모두

- 참조 투명성 유지
- 서술과 실행의 분리
- 타입 안전성 유지

를 주요 목표로 제시했다. 하지만 대체로 복잡도가 올라가거나 성능이 크게 하락하는 문제가 있었다.
특히 다양한 효과를 조합하는 환경에서는 복잡도가 크게 올라가서 실용성이 매우 떨어지는 문제가 있었다.

이러한 점들을 개선하여 위의 목표를 유지하되, 실용적인 방안을 제시하는 현대의 라이브러리 중 대표가 스칼라의 ZIO다.

ZIO의 주요 특징은 아래와 같다.

## 행위를 타입으로 표현

```scala
def getUser(id: UserId): ZIO[DB, Error, Option[User]]
```

- 위 코드는 id를 통해 user 정보를 불러오는 함수다.
- 실제 user 정보를 불러오는 행위는 즉시 실행되지 않으며 ZIO 객체가 평가될 때, 실행된다.
- DB는 해당 ZIO 객체를 실행하기 위해 필요한 의존성이다.
- Error, Option[User]는 각각 성공 실패 결과를 의미한다.
- DB는 실제 구현체가 아니며 실제 의존성을 주입하는 과정은 상위 레이어에서 수행한다.

## ZLayer

위 예시에서 `getUser`라는 함수는 DB 의존성을 요구한다. 하지만 실제로 모든 함수에 매번 의존성을 주입하는 것은 번거롭고 의존성 주입 책임을 한 곳으로 모으기 어렵다.

따라서 ZLayer라는 layer 단위로 의존성을 주입하는 방법이 제시된다.

```scala
// 현대적인 ZIO 방식
object UserRepo {
  // 서비스 정의
  trait Service {
    def getUser(id: UserId): ZIO[Any, Error, Option[User]]
  }

  // 실제 구현
  val live: ZLayer[Database, Nothing, UserRepo] =
    ZLayer.fromFunction { db =>
      new Service {
        def getUser(id: UserId) = /* 구현 */
      }
    }

  // 테스트용 구현
  val test: ZLayer[Any, Nothing, UserRepo] =
    ZLayer.succeed(new Service { /* 테스트 구현 */ })
}
```

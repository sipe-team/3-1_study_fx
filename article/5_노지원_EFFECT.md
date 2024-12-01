# ts에서 Effect 다루기 - Effect-ts

ZIO 라이브러리의 철학을 유지하며 ts에서 사용할 수 있는 라이브러리이다. nestjs의 기본 형태와 유사하게 예시를 작성했다.

```ts
// order.interface.ts
import { Context, Effect } from "effect";

import { Product } from "../product/product.interface";
import { User } from "../user/user.interface";

export interface Order {
  id: string;
  customer: User;
  product: Product;
  amount: number;
  total_price: number;
}

export interface OrderId {
  order_id: string;
}

export interface IOrderService {
  get: (input: { user_id: string; order_id: string }) => Effect.Effect<Order, "ORDER_NOT_FOUND" | "USER_NOT_FOUND">;
  create: (input: { user_id: string; product_id: string }) => Effect.Effect<OrderId, "PRODUCT_NOT_FOUND" | "USER_NOT_FOUND">;
}

export class OrderServiceToken extends Context.Tag("OrderService")<OrderServiceToken, IOrderService>() {}

// order.service.ts
import { Effect, Layer, pipe } from "effect";

import { IProductService, ProductServiceToken } from "../product/product.interface";
import { IUserService, UserServiceToken } from "../user/user.interface";
import { IOrderService, Order, OrderId, OrderServiceToken } from "./order.interfce";

export class OrderService implements IOrderService {
  constructor(private readonly userS: IUserService, private readonly productS: IProductService) {}

  static layer() {
    return Layer.effect(
      OrderServiceToken,
      pipe(
        Effect.all([UserServiceToken, ProductServiceToken]),
        Effect.map((props) => new OrderService(...props))
      )
    );
  }

  get(input: { user_id: string; order_id: string }): Effect.Effect<Order, "ORDER_NOT_FOUND" | "USER_NOT_FOUND"> {
    return Effect.gen(this, function* () {
      yield* this.userS.get(input);
      yield* Effect.fail("ORDER_NOT_FOUND" as const);
      return {} as Order;
    });
  }
  create(input: { user_id: string; product_id: string }): Effect.Effect<OrderId, "PRODUCT_NOT_FOUND" | "USER_NOT_FOUND"> {
    return Effect.gen(this, function* () {
      const user_effect = this.userS.get(input); // Effect.Effect<User, "USER_NOT_FOUND">
      const user: User = yield* user_effect; // User

      // 1. const user = await this.userService.get(input); Promise, Error Throw 방식
      // 2. const user = yield* this.userService.get(input); Effect, Error return 방식

      // comprehension 문법, 수학의 조건 제시법
      yield* this.productS.get(input);
      // return 결과는 Product, 실제 get의 결과는 Effect.Effect<Product, "PRODUCT_NOT_FOUND">

      // create order usecase
      return { order_id: "" };
    });
  }
}

// order.module.ts
import { Layer } from "effect";

import { ProductModule } from "../product/product.module";
import { UserModule } from "../user/user.module";
import { OrderService } from "./order.service";

export const OrderModule = Layer.mergeAll(OrderService.layer()).pipe(Layer.provide(ProductModule), Layer.provide(UserModule));

// api handler example
import { Effect, ManagedRuntime } from "effect";

import { Order, OrderServiceToken } from "./order/order.interfce";
import { OrderModule } from "./order/order.module";

const runtime = ManagedRuntime.make(OrderModule);

interface HttpBodyDTO {
  user_id: string;
  order_id: string;
}

export const apiHandler = async (body: HttpBodyDTO): Promise<Order> => {
  const usecase = Effect.flatMap(OrderServiceToken, (orderS) => orderS.get(body));
  return runtime.runPromise(usecase);
};
```

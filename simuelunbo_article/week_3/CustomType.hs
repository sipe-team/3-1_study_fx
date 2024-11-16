class CustomType a where
  customType :: a -> String            -- 타입 클래스를 

instance CustomType Int where
  customType x = "Int: " ++ show x  

instance CustomType Bool where
  customType True = "Yes"
  customType False = "No"

main :: IO ()
main = do
  putStrLn (customType (42 :: Int))    -- "Int: 42" 출력
  putStrLn (customType True)           -- "Yes" 출력
  putStrLn (customType False)          -- "No" 출력

-- 1.여러 타입에 대해 같은 인터페이스(customShow)를 정의
-- 2.각 타입별로 다른 구현을 제공
-- 3. 다형성(polymorphism)을 통해 같은 함수 이름으로 다른 동작을 수행
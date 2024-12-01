-- apply 2 정의

apply2 :: (a -> a) -> a -> a
apply2 f x = f $ f x

applyfn2 :: (a -> a) -> (a -> a)
applyfn2 f = f . f


main :: IO ()
main = do
    print $ apply2 sqrt 16.0
    print $ applyfn2 sqrt 16.0
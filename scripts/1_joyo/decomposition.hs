type TT = [Int]

tt :: TT
tt = [1, 2, 3, 4, 5]

x1 :: Int
x2 :: Int
xs :: TT
(x1:x2:xs) = tt

-- Recursive x:xs (x: 원소, xs: 나머지 배열) 로 분해를 통해 리스트 총 합을 구하는 예제
sum' list = 
    case list of
        [] -> 0
        x:xs -> x + sum' xs

main :: IO ()
main = do
    print x1
    print x2
    print xs
    print $ sum' [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

-- 마지막 원소 반환
myLast :: [a] -> a

myLast [] = error "empty list" -- empty list
myLast [x] = x -- single element list
myLast (_:xs) = myLast xs -- (head:tail) recursive call

myLast2 :: [a] -> a
myLast2 = head . reverse

-- 마지막에서 두번째 원소 반환
myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (x:xs) = myButLast xs

myButLast2 :: [a] -> a
myButLast2 = head . tail . reverse

-- 왼쪽 결합 연산자
infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

myButLast3 :: [a] -> a

myButLast3 xs = xs
    |> reverse
    |> tail
    |> head


-- 중복 원소 추가
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

-- 평균값 구하기
avg :: [Int] -> Float
avg list = sumElements / len
    where
        sumElements = fromIntegral(sum list) :: Float
        len = fromIntegral(length list) :: Float


---------------------------------------

main :: IO ()

main = do
    print (myLast [1])
    print (myLast [1,2,3,4,5])
    print (myLast "Hello")

    print (myLast2 [1])
    print (myLast2 [1,2,3,4,5])
    print (myLast2 "Hello")

    print (myButLast [1,2,3,4])
    print (myButLast ['a'..'z'])
    
    print (myButLast2 [1,2,3,4])
    print (myButLast2 ['a'..'z'])

    print (myButLast3 [1,2,3,4])
    print (myButLast3 ['a'..'z'])

    print (dupli [1,2,3])
    print (dupli "hello")
    
    print (avg [1,2,3, 4, 5, 6])
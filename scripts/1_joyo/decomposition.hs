type TT = [Int]

tt :: TT
tt = [1, 2, 3, 4, 5]

x1 :: Int
x2 :: Int
xs :: TT
(x1:x2:xs) = tt

main :: IO ()
main = do
    print x1
    print x2
    print xs

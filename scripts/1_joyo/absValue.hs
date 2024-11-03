-- 절대값 구하기

absValue :: Int -> Int
absValue n
    | n >= 0 = n
    | otherwise = (-n)

main :: IO ()
main = do
    print $ absValue 3
    print $ absValue (-5)
insertIn :: a -> [a] -> Int -> [a]

insertIn x ys 1 = x:ys
insertIn x (y : ys) n = y : insertIn x ys (n-1)
insertIn x [] _ = [x]

main :: IO ()
main = do 
    print ( insertIn 5 [9,9,9] 2 )
    print ( insertIn 5 [9,9,9] 10 )
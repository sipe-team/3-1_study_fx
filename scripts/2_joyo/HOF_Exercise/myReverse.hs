
--
----
------
{-

Title   | myReverse
Problem | Implement a function myReverse :: [Int] -> [Int] returns that reverses a list of integers
Example | myReverse [1..10] -- -> [10, 9, 8, ... 1] 
        | myReverse [6,2,4,8] -- -> [8,4,2,6]

-}
------
----
--

myReverse :: [Int] -> [Int]
myReverse = foldl (flip (:)) [] -- flip (:) x xs 가 xs x 순서로 뒤집힘
                                -- the same as foldl (\xs -> \x -> x : xs) []

main :: IO ()
main = do
        print $ myReverse [1..10] -- -> [10, 9, 8, ... 1] 
        print $ myReverse [6,2,4,8] -- -> [8,4,2,6]
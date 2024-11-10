
--
----
------
{-

Title   | countIn
Problem | Implement a function countIn :: [Int] -> Int -> [Int] returns that reverses a list of integers
Example | countIn [[3,2,3], [3], [], [2,2]] 3 -- -> [2,1,0,0]

-}
------
----
--

countIn :: [[Int]] -> Int -> [Int]
countIn l x = map (length . filter (==x)) l
         

main :: IO ()
main = do
        print $ countIn [[3,2,3], [3], [], [2,2]] 3 -- -> [2,1,0,0]
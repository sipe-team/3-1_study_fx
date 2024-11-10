
--
----
------
{-

Title   | flatten
Problem | Implement a function flatten :: [[Int]] -> [Int] that flattens a list of lists of integers in a list of integers
Example | flatten [[1,2,3],[4,5],[6],[],[3,3]]
        | flatten [[2,7],[8,9],[]] = 24

-}
------
----
--

flatten :: [[Int]] -> [Int]
flatten = foldr ((++)) []

main :: IO ()
main = do
        print $ flatten [[1,2,3],[4,5],[6],[],[3,3]]
        print $ flatten [[2,7],[8,9],[]]

--
----
------
{-

Title   | product
Problem | Implement a function prod :: [Int] -> Int that returns the product of a list of integers.
Example | prod [2,10,5] = 100
        | prod [3,1,2,4] = 24

-}
------
----
--

prod :: [Int] -> Int

prod = foldl (*) 1


main :: IO ()
main = do
        print $ prod [2,10,5]
        print $ prod [3,1,2,4]
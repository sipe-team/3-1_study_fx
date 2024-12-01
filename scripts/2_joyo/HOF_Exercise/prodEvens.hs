
--
----
------
{-

Title   | prodEvens
Problem | Implement a function prodEvens :: [Int] -> Int that returns the product of all even numbers of a list of integers.
Example | prodEvens [2,10,5] = 20
        | prodEvens [3,1,2,4] = 8

-}
------
----
--

prod :: [Int] -> Int
prod = foldl (*) 1

prodEvens :: [Int] -> Int

prodEvens = prod . filter even


main :: IO ()
main = do
        print $ prodEvens [2,10,5]
        print $ prodEvens [3,1,2,4]

--
----
------
{-

Title   | powerOf2
Problem | Implement a function powerOf2 :: [Int] that generates the list of all the powers of 2
Example | take 5 powerOf2 -> [1,2,4,8,16]
        | take 3 powerOf2 -> [1,2,4]

-}
------
----
--

powerOf2 :: [Int]
powerOf2 = map (2^) [0..]

powerOf2_2 = iterate (*2) 1

main :: IO ()
main = do
        print $ take 5 powerOf2
        print $ take 5 powerOf2_2
        print $ take 3 powerOf2
        print $ take 3 powerOf2_2
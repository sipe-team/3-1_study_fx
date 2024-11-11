
--
----
------
{-

Title   | countIf
Problem | Implement a function countIf :: (Int -> Bool) -> [Int] -> Int that, given a predicate on integers and a list of intergers, returns the number of elements in the list that satisfy the predicate.
Example | countIf (>5) [1..10] -- -> 5
        | countIf even [3,4,6,1] -- -> 2
-}
------
----
--

countIf :: (Int -> Bool) -> [Int] -> Int
countIf f = length . filter f
         

main :: IO ()
main = do
        print $ countIf (>5) [1..10]
        print $ countIf even [3,4,6,1]
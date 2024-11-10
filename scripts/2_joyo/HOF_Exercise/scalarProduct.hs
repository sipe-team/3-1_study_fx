
--
----
------
{-

Title   | scalarProduct
Problem | Implement a function scalarProduct :: [Float] -> [Float] -> Float that returns the dot product of two lists of float numbers with the same size.
Example | scalarProduct [2.0, 1.0, 5.0] [3.0, 2.0, 2.0] -> 18.0
        | scalarProduct [3.0, 4.0] [5.0, 3.0] -> 27.0

-}
------
----
--

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct l1 l2 = sum $ zipWith (*) l1 l2

main :: IO ()
main = do
        print $ scalarProduct [2.0,1.0,5.0] [3.0,2.0,2.0]
        print $ scalarProduct [3.0,4.0] [5.0,3.0]
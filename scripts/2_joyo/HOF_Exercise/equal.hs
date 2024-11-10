
--
----
------
{-

Problem | Implement a function eql :: [Int] -> [Int] -> Bool that tells whether two lists of integers are the same.
Example | eql [1,2,3] [1,2,3] = True
        | eql [1,2,3] [1,2,4] = False
        | eql [1,2,3] [1,2] = False
        | eql [1,2] [1,2,3] = False
        | eql [] [] = True
        | otherwise = False

-}
------
----
--

eql :: [Int] -> [Int] -> Bool

eql l1 l2
    | length l1 /= length l2 = False
    | otherwise = and $ zipWith (==) l1 l2


main :: IO ()
main = do
        print $ eql [1,2,3] [1,2,3]
        print $ eql [1,2,3] [1,2,4]
        print $ eql [1,2,3] [1,2]
        print $ eql [1,2] [1,2,3]
        print $ eql [] []
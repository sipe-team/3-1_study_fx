
--
----
------
{-

Title   | myLength
Problem | Implement a function myLength :: String -> Int that myLength that returns the length of String
Example | myLength 
        | myLength 

-}
------
----
--

myLength :: String -> Int
myLength = foldr ((+) . const 1) 0 -- the same as foldr (+) 0 . map (const 1)

main :: IO ()
main = do
        print $ myLength "Park" -- -> 4
        print $ myLength "Science" -- -> 7
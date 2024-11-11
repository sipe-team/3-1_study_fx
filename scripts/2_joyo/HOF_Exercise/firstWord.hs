
--
----
------
{-

Title   | firstWord
Problem | Implement a function firstWord :: String -> String that, given a string with blanks and alphabetic characters, returns its the first word.
Example | firstWord "   Good morning I say" -- --> "Good"

-}
------
----
--

firstWord :: String -> String
firstWord = takeWhile (/=' ') . dropWhile (== ' ')
         

main :: IO ()
main = do
        print $ firstWord "   Good morning I say"
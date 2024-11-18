
module StringUtils (
    capitalize,
    reverseString,
    countWords
) where

import Data.Char (toUpper)

capitalize :: String -> String
capitalize = map toUpper

reverseString :: String -> String
reverseString = foldl (flip (:)) []

countWords :: String -> Int
countWords = length
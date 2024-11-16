import Data.Char (toLower, isAlpha)


-- 왼쪽에서 오른쪽으로 함수를 합성하는 연산자
infixl 9 |>
(|>) :: (a -> b) -> (b -> c) -> (a -> c)
f |> g = g . f


{- 문자열을 받아서:
   1. 모든 문자를 소문자로 변환
   2. 공백을 제거
   3. 알파벳만 필터링
   4. 길이를 반환
-}


-- 기존 (.) 연산자 사용 (오른쪽에서 왼쪽으로 읽음)
countChars1 :: String -> Int
countChars1 = length . filter isAlpha . filter (/=' ') . map toLower

-- 새로운 (|>) 연산자 사용 (왼쪽에서 오른쪽으로 읽음)
countChars2 :: String -> Int
countChars2 = map toLower |> filter (/=' ') |> filter isAlpha |> length

main :: IO ()
main = do
    let text = "Hello World! 123"
    print $ countChars1 text  -- 10
    print $ countChars2 text  -- 10
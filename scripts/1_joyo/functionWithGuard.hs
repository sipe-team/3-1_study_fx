
{- 
  가드(guard) 문법은 함수의 조건부 실행을 위한 Haskell의 문법입니다.
   | 기호 뒤에 조건식을 작성하고, = 뒤에 해당 조건이 참일 때 실행될 표현식을 작성합니다.
   조건들은 위에서 아래로 순차적으로 평가되며, 첫 번째로 참이 되는 조건의 표현식이 실행됩니다.
   otherwise는 모든 조건이 거짓일 때 실행되는 기본 케이스입니다.
   보통 never 가 되지 않도록 otherwise 를 마지막에 작성하는 것을 권장합니다.
-}

getGrade :: Int -> Char
getGrade score
  | score >= 90 = 'A'
  | score >= 80 = 'B'
  | score >= 70 = 'C'
  | otherwise   = 'F'

main :: IO ()
main = do
    print $ getGrade 100
    print $ getGrade 80
    print $ getGrade 50
    print $ getGrade 30
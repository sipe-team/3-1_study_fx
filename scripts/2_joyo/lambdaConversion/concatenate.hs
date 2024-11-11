
{-# ANN module "HLint: ignore" #-}
-- (++) == concat == myConcat
myConcat = \xs -> \ys ->
    case xs of
        [] -> ys
        (x:xs') -> x : ((++) xs' ys)

main :: IO ()
main = do
    print $ myConcat [1,2,3] [4,5,6]
    print $ myConcat "he" "llo"
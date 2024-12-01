
{-# ANN module "HLint: ignore" #-}
zipWith1 = \f -> \xs -> \ys ->
    case (xs, ys) of
        ([], _) -> []
        (_, []) -> []
        (x:xs', y:ys') -> f x y : zipWith1 f xs' ys'

main :: IO ()
main = do
    print $ zipWith1 (+) [1,2,3] [4,5,6]
    print $ zipWith (+) [1,2,3] [4,5,6]
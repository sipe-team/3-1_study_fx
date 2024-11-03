import Data.Tuple (swap)

first (x, _, _) = x
second (_, y, _) = y
third (_, _, z) = z

main :: IO ()
main = do 
    print $ fst (3, 5)
    print $ snd (3, 5)
    print $ swap (3, 5)
    print $ curry (\(x, y) -> x + y) 3 5
    print $ uncurry (+) (3, 5)

    print $ first (3, 5, 7)
    print $ second (3, 5, 7)
    print $ third (3, 5, 7)

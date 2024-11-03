type Point = (Int, Int)
type Person = (String, Int)
type BookInto = (String, String, Int) -- 제목, 저자, 페이지수

point :: Point
point = (13, 24)

person :: Person
person = ("Joyo", 33)

bookInfo :: BookInto
bookInfo = ("Concept Lecture", "HOSODA", 333)

-- 2D 거리 계산
type Point2D = (Double, Double)

getDistance :: Point2D -> Point2D -> Double

getDistance (x1, y1) (x2, y2) = sqrt (dx ** 2 + dy ** 2)
    where
        dx = x2 - x1
        dy = y2 - y1

getDistance2 p1 p2 = sqrt (sqr dx + sqr dy)
    where
        (x1, y1) = p1
        (x2, y2) = p2
        dx = x2 - x1
        dy = y2 - y1
        sqr x = x * x

point1 :: Point2D
point1 = (0.0, 0.0)

point2 :: Point2D
point2 = (3.0, 4.0)

-- 학생 성적 관리
type Student = (String, Int, [Int]) -- (이름, 학번, 점수리스트)

-- 학생 점수를 받아서 학생 이름과 점수평균을 반환
getAvg :: Student -> (String, Double)
getAvg (name, _, scores) = (name, avg)
    where
        avg = fromIntegral (sum scores) / fromIntegral (length scores)

processStudents :: [Student] -> [(String, Double)]
processStudents = map getAvg

students :: [Student]
students = [
    ("Alice", 1001, [90, 85, 95]),
    ("Bob", 1002, [75, 80, 85]),
    ("Charlie", 1003, [95, 90, 100])
    ]


main :: IO ()
main = do
    print point
    print person
    print bookInfo
    print $ getDistance point1 point2
    print $ getDistance2 point1 point2
    mapM_ (\(name, avg) -> 
        putStrLn $ name ++ "'s average: " ++ show avg) (processStudents students)

import qualified Data.Text    as T
import qualified Data.Text.IO as T

text2Int :: T.Text -> Int
text2Int x = read $ T.unpack x

main = do
    ls <- fmap T.lines (T.readFile "day1.txt")
    print (cntBiggerThan (map text2Int ls))

cntBiggerThan :: [Int] -> Int
cntBiggerThan []       = 0
cntBiggerThan [x]      = 0
cntBiggerThan (x:y:zs) =
    if x < y then 1 + cntBiggerThan (y:zs)
    else cntBiggerThan (y:zs)

average3 :: [Int] -> [Int]
average3 []  = []
average3 [x] = []
average3 [x, y] = []
average3 (x:y:z:qs) = (x+y+z) : average3 (y:z:qs)

main2 = do
    ls <- fmap T.lines (T.readFile "day1.txt")
    print (cntBiggerThan (average3 (map text2Int ls)))
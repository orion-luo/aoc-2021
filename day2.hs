import qualified Data.Text    as T
import qualified Data.Text.IO as T

text2String :: T.Text -> String
text2String = T.unpack

main = do
    ls <- fmap T.lines (T.readFile "day2.txt")
    print (goPlaces (map (getDirections . T.unpack) ls) (0, 0))

getDirections :: String -> (String, Int)
getDirections s = (head ls, read $ last ls)
    where ls = words s

goPlaces :: [(String, Int)] -> (Int, Int) -> (Int, Int)
goPlaces [] x = x
goPlaces ((x, y) : zs) (horizontal, depth)
  | x == "forward" = goPlaces zs (horizontal+y, depth)
  | x == "up"      = goPlaces zs (horizontal, depth-y)
  | otherwise      = goPlaces zs (horizontal, depth+y)

goPlacesProper :: [(String, Int)] -> (Int, Int, Int) -> (Int, Int, Int)
goPlacesProper [] x = x
goPlacesProper ((x, y) : zs) (horizontal, depth, aim)
  | x == "down"    = goPlacesProper zs (horizontal, depth, aim+y)
  | x == "up"      = goPlacesProper zs (horizontal, depth, aim-y)
  | otherwise      = goPlacesProper zs (horizontal+y, depth+aim*y, aim)

main2 = do
    ls <- fmap T.lines (T.readFile "day2.txt")
    print (goPlacesProper (map (getDirections . T.unpack) ls) (0, 0, 0))
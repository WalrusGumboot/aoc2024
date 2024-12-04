import Data.List (isInfixOf, transpose)
import System.Environment (getArgs)
import Control.Monad (join)
import Data.Bifunctor (bimap)

getCoordsForSW :: Int -> Int -> [(Int, Int)]
getCoordsForSW n sum =
  let base = zip [0 .. sum] [sum, sum - 1 .. 0]
   in filter (\(x, y) -> x < n && y < n) base

mirrorX :: Int -> (Int, Int) -> (Int, Int)
mirrorX n (x, y) = ((n - 1) - x, y)

at :: [[a]] -> (Int, Int) -> a
at l (x, y) = l !! y !! x

fromCoords :: [[a]] -> [[(Int, Int)]] -> [[a]]
fromCoords grid = map (map (grid `at`))

windows :: [a] -> Int -> [[a]]
windows xs n = map (\i -> take n $ drop i xs) [0 .. length xs - n]

countInfixes :: (Eq a) => [a] -> [a] -> Int
countInfixes needle haystack =
  let ws = windows haystack (length needle)
   in length $ filter (== needle) ws

main :: IO ()
main = do
  args <- getArgs
  raw <- readFile (head args)
  let grid = lines raw
  let n = length (head grid)

  let horizontals = grid
  let verticals = transpose grid
  let diagonalsSW = fromCoords grid $ map (getCoordsForSW n) [0 .. 2 * n]
  let diagonalsSE = fromCoords grid $ map (map (n `mirrorX`) . getCoordsForSW n) [0 .. 2 * n]

  let allStrings = horizontals ++ verticals ++ diagonalsSW ++ diagonalsSE

  let matchesForward = sum (map (countInfixes "XMAS") allStrings)
  let matchesBackward = sum (map (countInfixes "SAMX") allStrings)

  let pt1 = matchesForward + matchesBackward

  putStrLn $ "part 1: " ++ show pt1

  let blockCoords = (,) <$> [1 .. n-2] <*> [1 .. n-2]
  let xShapes = map (\(x, y) -> 
        join bimap (map (grid `at`)) 
        ([(x - 1, y - 1), (x, y), (x + 1, y + 1)], 
         [(x - 1, y + 1), (x, y), (x + 1, y - 1)])) 
        blockCoords

  let pt2 = length $ filter (\(a, b) -> (a == "MAS" || a == "SAM") && (b == "MAS" || b == "SAM")) xShapes

  putStrLn $ "part 2: " ++ show pt2
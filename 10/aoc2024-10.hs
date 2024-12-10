import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, maybe)
import System.Environment (getArgs)
import Data.List (nub)

expand :: Map (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
expand grid pos@(x, y) = let north = (x, y - 1)
                             east  = (x + 1, y)
                             south = (x, y + 1)
                             west  = (x - 1, y)
                             dirs  = [north, east, south, west] 
                             self  = fromJust (Map.lookup pos grid)
                      in filter (\p -> maybe False (\v -> v == self + 1) (Map.lookup p grid)) dirs

expandHike :: Map (Int, Int) Int -> [(Int, Int)] -> [[(Int, Int)]]
expandHike grid hike =
  let extensions = map (\e -> hike ++ [e]) (expand grid (last hike))
   in if null extensions
        then []
        else
          if length (head extensions) == 10
            then extensions
            else concatMap (expandHike grid) extensions

score :: Map (Int, Int) Int -> (Int, Int) -> Int
score grid trailhead = length $ nub $ map last (expandHike grid [trailhead])

rating :: Map (Int, Int) Int -> (Int, Int) -> Int
rating grid trailhead = length $ expandHike grid [trailhead]

main :: IO ()
main = do
  args <- getArgs
  raw <- readFile (head args)

  let gridList :: [[Int]] = map (map (read . (: []))) (lines raw)
  let grid = Map.fromList $ concatMap (\y -> map (\x -> ((x, y), gridList !! y !! x)) [0 .. length (gridList !! y) - 1]) [0 .. length gridList - 1]

  let trailheads = Map.keys $ Map.filter (== 0) grid

  -- mapM_ (\z -> putStrLn $ show z ++ " score " ++ show (score grid z)) zeroSpots

  let pt1 = sum $ map (score grid) trailheads
  putStrLn $ "part 1: " ++ show pt1

  let pt2 = sum $ map (rating grid) trailheads
  putStrLn $ "part 2: " ++ show pt2

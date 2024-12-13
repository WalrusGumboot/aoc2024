{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

import Control.Monad (replicateM)
import Control.Monad.State
import Data.Char (chr)
import Data.List (nub, sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, isJust, mapMaybe)
import System.Environment (getArgs)
import Debug.Trace (traceM, trace)
import Data.Foldable (traverse_)

type Coord = (Int, Int)

type Grid a = Map (Int, Int) a

insertMany :: (Ord k) => Map k v -> [(k, v)] -> Map k v
insertMany = foldl (\acc (k, v) -> Map.insert k v acc)

-- floodFill_ :: (Eq a) => Grid a -> Coord -> [Coord] -> [Coord]
-- floodFill_ grid o@(x, y) visited =
--   let possibilities = [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]
--       neighbours = mapMaybe (\c -> (\v -> Just (c, v)) =<< Map.lookup c grid) possibilities
--       original = fromJust $ Map.lookup o grid
--       sameNeighbours = [c | (c, v) <- neighbours, v == original, c `notElem` visited]
--    in trace ("floodFill_ pass with " ++ show sameNeighbours) (o : sameNeighbours ++ (sameNeighbours >>= (\c -> floodFill_ grid c (o : c : visited))))

-- floodFill :: (Eq a, Ord a) => Grid a -> Coord -> [Coord]
-- floodFill grid (x, y) = (sort . nub) $ floodFill_ grid (x, y) []

floodFill_ :: (Eq a) => Grid a -> Coord -> State [Coord] ()
floodFill_ grid o@(x, y) = do
    visited <- get
    let possibilities = [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]
    let neighbours = mapMaybe (\c -> (\v -> Just (c, v)) =<< Map.lookup c grid) possibilities
    let original = fromJust $ Map.lookup o grid
    let sameNeighbours = [c | (c, v) <- neighbours, v == original, c `notElem` visited]
    -- traceM $ "call to floodFill_ with" ++ show sameNeighbours
    put $ visited ++ sameNeighbours
    traverse_ (floodFill_ grid) sameNeighbours

floodFill :: (Eq a) => Grid a -> Coord -> [Coord]
floodFill grid coord = execState (floodFill_ grid coord) [coord]

type RegionIdx = Int

type RegionState = (RegionIdx, Map Coord RegionIdx)

assignRegion :: (Eq a, Ord a) => Grid a -> Coord -> State RegionState RegionIdx
assignRegion grid coord = do
  (highestIdx, currentMap) <- get
--   traceM $ "assigning region to " ++ show coord
  case Map.lookup coord currentMap of
    Nothing -> do
      put (highestIdx + 1, insertMany currentMap (map (\c -> (c, highestIdx)) (floodFill grid coord)))
    --   traceM "  was not yet in map"
      pure highestIdx
    Just i -> pure i

assignGrid :: (Eq a, Ord a) => Grid a -> [Coord] -> State RegionState RegionIdx
assignGrid _ [] = do
  (idx, _) <- get
  pure idx
assignGrid grid (c : cs) = do
  reg <- assignRegion grid c
  assignGrid grid cs
  pure reg

{-
note that this methods makes sure that the ks are sorted!
i.e. for coordinates, this is in reading order
-}
invertMap :: (Ord k, Ord v) => Map k v -> Map v [k]
invertMap m = Map.fromList [(v, ks) | v <- Map.elems m, let ks = filter (\k -> case Map.lookup k m of Nothing -> False; Just v' -> v' == v) (Map.keys m)]

{-
we calculate this by first pretending every cell has
all edges and then subtracting the unnecessary internal
edges, by traversing cells in reading order and removing
edges to the right or downwards respectively if there's a cell there
-}
perimeter :: [Coord] -> Int
perimeter region =
  4 * length region
    - sum (map (\(x, y) -> let right = (x + 1, y) in if right `elem` region then 2 else 0) region)
    - sum (map (\(x, y) -> let below = (x, y + 1) in if below `elem` region then 2 else 0) region)

main :: IO ()
main = do
  args <- getArgs
  raw <- readFile (head args)

  let gridList = lines raw
  let grid = Map.fromList $ concatMap (\y -> map (\x -> ((x, y), gridList !! y !! x)) [0 .. length (gridList !! y) - 1]) [0 .. length gridList - 1]

  let allCoords = [(x, y) | y <- [0 .. length gridList - 1], x <- [0 .. length (gridList !! y) - 1]]

  -- mapM_ (\c -> putStrLn $ show c ++ ": " ++ show (floodFill grid c)) allCoords

  let (_, stateAssignment) = execState (assignGrid grid allCoords) (0, Map.empty)
  let regions = invertMap stateAssignment

  -- print regions

  -- mapM_ (\(rIdx, r) -> putStrLn $ chr (rIdx + 65) : ": perimeter " ++ show (perimeter r) ++ ", area " ++ show (length r) ++ ", price: " ++ show (length r * perimeter r)) (Map.toList regions)

  let pt1 = sum $ map (\(_, r) -> perimeter r * length r) (Map.toList regions)
  
  putStrLn $ "part 1: " ++ show pt1
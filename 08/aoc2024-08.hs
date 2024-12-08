import Control.Monad (join)
import Data.List (nub)
import Data.Maybe (catMaybes)
import System.Environment (getArgs)

coordsOf :: (Eq a) => [[a]] -> a -> [(Int, Int)]
coordsOf grid el =
  catMaybes
    ( join $
        map
          ( \y ->
              map
                ( \x -> if grid !! y !! x == el then Just (x, y) else Nothing
                )
                [0 .. length (head grid) - 1]
          )
          [0 .. length grid - 1]
    )

nthOffset :: (Int, Int) -> (Int, Int) -> Int -> (Int, Int)
nthOffset (aX, aY) (bX, bY) n = (n * aX - (n - 1) * bX, n * aY - (n - 1) * bY)

main :: IO ()
main = do
  args <- getArgs
  raw <- readFile (head args)

  let grid = map (map (\c -> if c == '.' then Nothing else Just c)) (lines raw)
  let w = length (head grid)
  let h = length grid

  let uniqueFreqs = nub $ catMaybes (join grid)

  let coordSets = map (coordsOf grid . Just) uniqueFreqs
  let antinodes = join $ map (\l -> [nthOffset a b 2 | a@(aX, aY) <- l, b@(bX, bY) <- l, a /= b]) coordSets  

  let pt1 = length $ nub $ filter (\(x, y) -> x >= 0 && y >= 0 && x < w && y < h) antinodes

  -- very dumb algorithm, can't be arsed to do this more intelligently
  let manyAntinodes = join $ map (\l -> concatMap (\i -> [nthOffset a b i | a@(aX, aY) <- l, b@(bX, bY) <- l, a /= b]) [1 .. w `max` h]) coordSets  
  let pt2 = length $ nub $ filter (\(x, y) -> x >= 0 && y >= 0 && x < w && y < h) manyAntinodes


  putStrLn $ "part 1: " ++ show pt1
  putStrLn $ "part 2: " ++ show pt2
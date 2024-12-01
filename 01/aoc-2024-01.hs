import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.List (break, sort, unzip)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  raw <- readFile (head args)
  let ls = lines raw

  let (as, bs) :: ([Int], [Int]) = join bimap sort $ unzip $ map (\s -> let p = break (== ' ') s in join bimap read p) ls

  let pt1 = sum $ zipWith (\a b -> abs $ a - b) as bs
  let pt2 = sum $ map (\a -> a * length (filter (== a) bs)) as

  putStrLn $ "part 1: " ++ show pt1
  putStrLn $ "part 2: " ++ show pt2

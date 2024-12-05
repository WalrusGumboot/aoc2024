import Data.List (elemIndex, find, findIndex, sortBy)
import Data.Maybe (fromJust)
import System.Environment (getArgs)

splitDelim :: Char -> String -> [String]
splitDelim d "" = []
splitDelim d s =
  let (a, b) = span (/= d) s
   in a : splitDelim d (trim b)
  where
    trim s = if null s || head s /= d then s else tail s

pageOrder :: [(Int, Int)] -> Int -> Int -> Ordering
pageOrder orderings a b
  | (a, b) `elem` orderings = LT
  | (b, a) `elem` orderings = GT
  | otherwise = EQ

isCorrectlyOrdered :: [(Int, Int)] -> [Int] -> Bool
isCorrectlyOrdered orderings pages = fixPages orderings pages == pages

fixPages :: [(Int, Int)] -> [Int] -> [Int]
fixPages orderings = sortBy (pageOrder orderings)

middleElems :: [[a]] -> [a]
middleElems = map (\l -> l !! (length l `div` 2))

main :: IO ()
main = do
  args <- getArgs
  raw <- readFile (head args)
  let ls = lines raw
  let orderingLines = takeWhile (/= "") ls
  let updateLines = tail $ dropWhile (/= "") ls
  let orderings :: [(Int, Int)] =
        map
          ( \s ->
              let (beforeSep, afterSep) = break (== '|') s
               in (read beforeSep, read $ tail afterSep)
          )
          orderingLines
  let updates :: [[Int]] = map (map read . splitDelim ',') updateLines
  let validUpdates = filter (isCorrectlyOrdered orderings) updates
  let pt1 = sum $ middleElems validUpdates
  putStrLn $ "part 1: " ++ show pt1

  let invalidUpdates = filter (not . isCorrectlyOrdered orderings) updates
  let pt2 = sum $ middleElems (map (fixPages orderings) invalidUpdates)
  putStrLn $ "part 2: " ++ show pt2
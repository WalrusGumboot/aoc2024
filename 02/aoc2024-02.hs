import System.Environment (getArgs)

splitDelim :: Char -> String -> [String]
splitDelim d "" = []
splitDelim d s =
  let (a, b) = span (/= d) s
   in a : splitDelim d (trim b)
  where
    trim s = if null s || head s /= d then s else tail s

onPairs :: (a -> a -> b) -> [a] -> [b]
onPairs f (a1 : (a2 : as)) = f a1 a2 : onPairs f (a2 : as)
onPairs _ _ = []

levelsOk :: [Int] -> Bool
levelsOk nums = let deltas = onPairs (-) nums
                    signs  = map signum deltas
                 in all (== head signs) (tail signs) 
                 && all (\d -> abs d `elem` [1, 2, 3]) deltas -- this also takes care of the case where all nums are equal

main :: IO ()
main = do
  args <- getArgs
  raw <- readFile (head args)
  let nums :: [[Int]] = map (map read . splitDelim ' ') $ lines raw
  
  let pt1 = length $ filter levelsOk nums
  let pt2 = length $ filter (\ns -> let removeds = map (\i -> take i ns ++ drop (i + 1) ns) [0..length ns] 
                                     in any levelsOk removeds) nums

  putStrLn $ "part 1: " ++ show pt1
  putStrLn $ "part 2: " ++ show pt2
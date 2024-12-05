import System.Environment (getArgs)

splitDelim :: Char -> String -> [String]
splitDelim d "" = []
splitDelim d s =
  let (a, b) = span (/= d) s
   in a : splitDelim d (trim b)
  where
    trim s = if null s || head s /= d then s else tail s


extract :: [a] -> Int -> ([a], a, [a])
extract xs i = (take i xs, xs !! i, tail $ drop i xs)

isCorrectlyOrdered :: [(Int, Int)] -> [Int] -> Bool
isCorrectlyOrdered orderings pages = all (\i -> let (b, e, a) = extract pages i
                                                    shouldBeBefore = [before | (before, e) <- orderings ]
                                                    shouldBeAfter  = [after  | (e, after)  <- orderings ]
                                                 in all (`elem` shouldBeBefore) b && all (`elem` shouldBeAfter) a) [0..length pages - 1]

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
  let middleElems  = map (\l -> l !! (length l `div` 2)) validUpdates
  print middleElems
  let pt1 = sum middleElems

  putStrLn $ "part 1: " ++ show pt1
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import System.Environment (getArgs)

splitDelim :: Char -> String -> [String]
splitDelim d "" = []
splitDelim d s =
  let (a, b) = span (/= d) s
   in a : splitDelim d (trim b)
  where
    trim s = if null s || head s /= d then s else tail s

blink :: Integer -> Seq Integer
blink a
  | a == 0 = Seq.singleton 1
  | (even . length . show) a = let s = show a; (x, y) = splitAt (length s `div` 2) s in Seq.fromList [read x :: Integer, read y :: Integer]
  | otherwise = Seq.singleton (a * 2024)

blinkAll :: Seq Integer -> Seq Integer
blinkAll = foldl (\a b -> a Seq.>< blink b) Seq.empty

main :: IO ()
main = do
  args <- getArgs
  raw <- readFile (head args)

  let stones :: Seq Integer = Seq.fromList $ map read (splitDelim ' ' raw)

  -- let pt1 = (length . last . take 26) (iterate blinkAll stones)
  -- putStrLn $ "part 1: " ++ show pt1

  -- let pt2 = (length . last . take 76) (iterate blinkAll stones)
  -- putStrLn $ "part 2: " ++ show pt2

  mapM_ (\(i, d) -> putStrLn $ show i ++ " : " ++ show (length d)) (zip [0 ..] (iterate blinkAll stones))
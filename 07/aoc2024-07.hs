import Control.Monad (replicateM)
import System.Environment (getArgs)

splitDelim :: Char -> String -> [String]
splitDelim d "" = []
splitDelim d s =
  let (a, b) = span (/= d) s
   in a : splitDelim d (trim b)
  where
    trim s = if null s || head s /= d then s else tail s

evaluableFor :: [Int -> Int -> Int] -> (Int, [Int]) -> Bool
evaluableFor operations (target, nums) =
  any
    ( \os ->
        foldl
          (\acc (new, f) -> f acc new)
          (head nums)
          (zip (tail nums) os)
          == target
    )
    (replicateM (length nums - 1) operations)

main :: IO ()
main = do
  args <- getArgs
  raw <- readFile (head args)

  let targets :: [(Int, [Int])] =
        map
          ( \l ->
              let (bef, aft) = break (== ':') l
               in ( read bef,
                    map read (splitDelim ' ' (drop 2 aft))
                  )
          )
          (lines raw)

  let pt1 = sum (map fst (filter (evaluableFor [(+), (*)]) targets))
  putStrLn $ "part 1: " ++ show pt1

  let pt2 = sum (map fst (filter (evaluableFor [(+), (*), \x y -> read (show x ++ show y)]) targets))
  putStrLn $ "part 2: " ++ show pt2

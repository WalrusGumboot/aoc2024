import Control.Monad (guard)
import Data.Char (isDigit)
import System.Environment (getArgs)

extractMul :: String -> Maybe (Int, Int)
extractMul s = do
  guard (take 4 s == "mul(")

  let a = takeWhile isDigit (drop 4 s)
  guard (not (null a))
  guard (s !! (length a + 4) == ',')

  let b = takeWhile isDigit (drop (4 + length a + 1) s)
  guard (not (null b))

  let totalLen = length a + length b + 6
  guard (s !! (totalLen - 1) == ')')

  pure (read a * read b, totalLen)

parse :: String -> [Int]
parse [] = []
parse s = case extractMul s of
  Nothing -> parse $ tail s
  Just (mult, idx) -> mult : parse (drop idx s)

eraseDeadText :: String -> Bool -> String
eraseDeadText ('d' : 'o' : '(' : ')' : ss) _ = eraseDeadText ss True
eraseDeadText ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : ss) _ = eraseDeadText ss False
eraseDeadText (s : ss) True = s : eraseDeadText ss True
eraseDeadText (s : ss) False = eraseDeadText ss False
eraseDeadText "" _ = ""

main :: IO ()
main = do
  args <- getArgs
  raw <- readFile (head args)

  let pt1 = sum $ parse raw
  let pt2 = sum $ parse $ eraseDeadText raw True

  putStrLn $ "part 1: " ++ show pt1
  putStrLn $ "part 2: " ++ show pt2
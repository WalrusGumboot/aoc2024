{-# LANGUAGE LambdaCase #-}

import Data.List (findIndex)
import Data.Maybe (fromJust, isNothing, mapMaybe)
import System.Environment (getArgs)

step1 :: [Maybe Int] -> [Maybe Int]
step1 d = case findIndex isNothing d of
  Nothing -> d
  Just blankIdx -> take blankIdx d ++ [last d] ++ init (drop (blankIdx + 1) d)

findFirstRep :: (Eq a) => [a] -> Maybe a
findFirstRep (x : y : rest) = if x == y then Just x else findFirstRep (y : rest)
findFirstRep _ = Nothing

data Span = File Int Int | Void Int deriving (Show, Eq)

isVoid :: Span -> Bool
isVoid (Void _) = True
isVoid _ = False

step2 :: [Span] -> [Span]
step2 [] = []
step2 [a] = [a]
step2 d = case last d of
  v@(Void n) -> step2 (init d) ++ [v]
  f@(File ln idx) -> case findIndex (\case Void n -> n >= ln; _ -> False) d of
                    Nothing -> step2 (init d) ++ [f]
                    Just pos -> let voidLen = case d !! pos of Void n -> n; _ -> error "found File which should be Void"
                                 in take pos d ++ [f] ++ [Void $ voidLen - ln | voidLen > ln] ++ init (drop (pos + 1) d) ++ [Void ln]
                                 

showSpan :: Span -> String
showSpan (Void n) = replicate n '.'
showSpan (File n i) = concat $ replicate n (show i)

showDisc :: [Span] -> String
showDisc = concatMap showSpan

main :: IO ()
main = do
  args <- getArgs
  raw <- readFile (head args)
  let nums = map (read . (: [])) raw

  let disc1 =
        let spacings = zipWith (\f v -> f v) (cycle [Right, Left]) nums
         in concatMap (\(s, idx) -> either (`replicate` Nothing) (`replicate` Just idx) s) (zip spacings (map (`div` 2) [0 ..]))
  let fp1 = fromJust $ findFirstRep (iterate step1 disc1)
  let pt1 = sum $ zipWith (*) (map fromJust fp1) [0 ..]

--   putStrLn $ "part 1: " ++ show pt1

  let disc2 =
        let spacings = zipWith (\f v -> f v) (cycle [Right, Left]) nums
         in zipWith (\s idx -> either Void (`File` idx) s) spacings (map (`div` 2) [0 ..])
  
  mapM_ (putStrLn . showDisc) (iterate step2 disc2)
  let fp2 = fromJust $ findFirstRep (iterate step2 disc2)
  let fp2AsNums = concatMap (\case Void n -> replicate n 0; File n idx -> replicate n idx) fp2
  let pt2 = sum $ zipWith (*) fp2AsNums [0..]
  return ()
--   putStrLn $ "part 2: " ++ show pt2


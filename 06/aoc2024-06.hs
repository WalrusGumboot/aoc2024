import Control.Monad (join)
import Data.List (findIndex)
import Data.Maybe (fromJust)
import System.Environment (getArgs)

data Dir = North | East | South | West

toChar :: Dir -> Char
toChar North = '^'
toChar East = '>'
toChar South = 'v'
toChar West = '<'

fromChar :: Char -> Dir
fromChar '^' = North
fromChar '>' = East
fromChar 'v' = South
fromChar '<' = West

rotate :: Dir -> Dir
rotate North = East
rotate East = South
rotate South = West
rotate West = North

guardChars :: [Char] = ['^', '>', 'v', '<']

data Cell = Open | Blocked | Visited deriving (Eq)

data Guard = Guard
  { guardPos :: (Int, Int),
    facing :: Dir
  }

data Board = Board
  { cells :: [[Cell]],
    guard :: Guard
  }

move :: Board -> Either Int Board
move Board {cells = c, guard = Guard {guardPos = g, facing = f}} =
  let n = length (head c)
      inFront = case f of
        North -> (fst g, snd g - 1)
        East -> (fst g + 1, snd g)
        South -> (fst g, snd g + 1)
        West -> (fst g - 1, snd g)
   in if any (\i -> i < 0 || i >= n) [fst inFront, snd inFront]
        then Left $ length (filter (== Visited) (join c)) + 1 -- we add one to count the 'leaving' move
        else
          let cellInFront = (c !! snd inFront) !! fst inFront
           in case cellInFront of
                Blocked -> Right $ Board {cells = c, guard = Guard {guardPos = g, facing = rotate f}}
                _ -> Right $ Board {cells = c', guard = Guard {guardPos = inFront, facing = f}}
                  where
                    c' = map (\y -> map (\x -> if (x, y) == g then Visited else c !! y !! x) [0 .. n - 1]) [0 .. n - 1]

instance Show Board where
  show Board {cells = c, guard = Guard {guardPos = g, facing = f}} =
    let n = length c
     in unlines $
          map
            ( \y ->
                map
                  ( \x ->
                      if (x, y) == g
                        then toChar f
                        else case c !! y !! x of
                          Open -> '.'
                          Blocked -> '#'
                          Visited -> 'X'
                  )
                  [0 .. n - 1]
            )
            [0 .. n - 1]

iterateMove :: Board -> Int
iterateMove board = either id iterateMove (move board)

iterateMove' :: Board -> Board
iterateMove' board = either (const board) iterateMove' (move board)

main :: IO ()
main = do
  args <- getArgs
  raw <- readFile (head args)
  let ls = lines raw

  let board =
        Board
          { cells = map (map (\c -> if c == '#' then Blocked else Open)) ls,
            guard =
              let gY = fromJust $ findIndex (\l -> any (`elem` l) guardChars) ls
                  gX = fromJust $ findIndex (`elem` guardChars) (ls !! gY)
                  dir = case (ls !! gY) !! gX of
                    '^' -> North
                    '>' -> East
                    'v' -> South
                    '<' -> West
               in Guard {guardPos = (gX, gY), facing = dir}
          }

  --   print $ iterateMove' board
  print $ iterateMove board

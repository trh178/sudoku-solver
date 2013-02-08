import Data.Char (digitToInt)
import Data.List

--type Solution = [Int]
type Possibility = [Int]
type Board = [Possibility] 

--rowSweep :: Board -> Board

--colSweep :: Board -> Board

--boxSweep :: Board -> Board

--solutionUpdate :: Board -> Solution

--solutionCheck :: Solution -> Bool

initBoard :: Board
initBoard = take 81 $ repeat $ [1..9]

populateBoard :: Board -> String -> Board
populateBoard [] _ = []
populateBoard _ [] = []
populateBoard (b:bs) (s:ss)
  | cell > 0 = [digitToInt s] : populateBoard bs ss 
  | otherwise = b : populateBoard bs ss
  where cell = digitToInt s
    
main :: IO ()
main = do
  lines <- sequence $ take 9 $ repeat getLine
  let board = populateBoard initBoard (concat lines)

  print board
  


import Data.Char (digitToInt)
import Data.List

--type Solution = [Int]
type Possibility = [Int]
type Row = [Possibility]
type Board = [Possibility] 

--rowSweep :: Row -> Row

--colSweep :: Board -> Board

--boxSweep :: Board -> Board

checkRows :: Board -> Board
checkRows [] = []
checkRows (r:rs) = undefined

finished :: Board -> Bool
finished [] = True
finished (b:bs)
  | length b > 1 = False
  | otherwise = finished bs

solveBoard :: Board -> Board
solveBoard b
  | finished b = b
  | otherwise = checkRows b

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
  let solution = solveBoard board

  print board
  


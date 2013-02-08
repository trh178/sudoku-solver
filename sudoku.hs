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
populateBoard (b:bs) (s:ss) = delete (digitToInt s) b : populateBoard bs ss 

main :: IO ()
main = do
  rs1 <- getLine
  rs2 <- getLine
  rs3 <- getLine
  rs4 <- getLine
  rs5 <- getLine
  rs6 <- getLine
  rs7 <- getLine
  rs8 <- getLine
  rs9 <- getLine

  let board = populateBoard initBoard boardString
        where boardString = rs1 ++ rs2 ++ rs3 ++ rs4 ++ rs5 ++ rs6 ++ rs7 ++ rs8 ++ rs9

  print board
  


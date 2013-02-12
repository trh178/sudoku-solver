import Data.Char (digitToInt)
import Data.List


--type Solution = [Int]
type Possibility = [Int]
type Row = [Possibility]
type Col = [Possibility]
type Box = [Possibility]
type Board = [Possibility] 


-- HELPERS
everyf :: Int -> [a] -> [a]
everyf n [] = []
everyf n as = head as : everyf n (drop n as)
every n k = everyf n . drop (k-1)


-- MODULE FUNCTIONS

extractRow :: Board -> Int -> Row
extractRow b i = take 9 $ drop (i - 1) $ b

extractCol :: Board -> Int -> Col
extractCol b i = every 9 i b

extractBox :: Board -> Int -> Box
extractBox = undefined

rowSweep :: Row -> Row
rowSweep r =
  let singles = concat $ filter (null . tail) r
  in map (\x -> if length x > 1 then x \\ singles else x) r

colSweep :: Col -> Col
colSweep = undefined

boxSweep :: Box -> Box
boxSweep = undefined

checkRows :: Board -> Board
checkRows [] = []
checkRows b = union (rowSweep r) (checkRows rs)
          where r = take 9 b
                rs = drop 9 b

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


-- MAIN
main :: IO ()
main = do
  lines <- sequence $ take 9 $ repeat getLine
  let board = populateBoard initBoard (concat lines)
  let solution = solveBoard board
  
  print board
  


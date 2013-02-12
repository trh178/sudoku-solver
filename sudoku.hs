import Data.Char (digitToInt)
import Data.List


--type Solution = [Int]
type Possibility = [Int]
type Region = [Possibility]
type Board = [Possibility] 


-- HELPERS
everyf :: Int -> [a] -> [a]
everyf n [] = []
everyf n as = head as : everyf n (drop n as)
every n k = everyf n . drop (k-1)

teveryf :: Int -> [a] -> [a]
teveryf n [] = []
teveryf n as = union (take 3 as) (teveryf n (drop n as))
tevery n k = teveryf n . drop (k - 1)


-- MODULE FUNCTIONS

extractRow :: Board -> Int -> Region
extractRow b i = take 9 $ drop (i - 1) $ b

extractCol :: Board -> Int -> Region
extractCol b i = every 9 i b

extractBox :: Board -> Int -> Region
extractBox b i = take 9 $ tevery 9 k b
    where k = [1,4,7,28,31,34,55,58,61] !! (i-1)

s_transpose :: Board -> Board
s_transpose b = helper b 1
    where helper b i
            | i > 9 = []
            | otherwise = union (extractCol b i) (helper b (i + 1))

s_btransform :: Board -> Board
s_btransform b = helper b 1
    where helper b i
             | i > 9 = []
             | otherwise = union (extractBox b i) (helper b (i + 1))

sweep :: Region -> Region
sweep r =
    let singles = concat $ filter (null . tail) r
    in map (\x -> if length x > 1 then x \\ singles else x) r

finished :: Board -> Bool
finished [] = True
finished (b:bs)
    | length b > 1 = False
    | otherwise = finished bs

checkRows :: Board -> Board
checkRows [] = []
checkRows b = union (sweep r) (checkRows rs)
    where r = take 9 b
          rs = drop 9 b

checkCols :: Board -> Board
checkCols = s_transpose . checkRows . s_transpose

checkBoxes :: Board -> Board
checkBoxes = s_btransform . checkRows . s_btransform

solveBoard :: [(Board -> Board)] -> (Board -> Bool) -> Board -> Board
solveBoard (f:fs) term board
    | term nextBoard = nextBoard
    | otherwise = solveBoard fs term nextBoard
    where nextBoard = f board

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
  let solution = solveBoard (cycle [checkRows, checkCols, checkBoxes]) finished board
  
  print board
  


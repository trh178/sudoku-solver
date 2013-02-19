import Data.Char (digitToInt)
import Data.List

type Possibility = [Int]
type Region = [Possibility]
type Board = [Possibility] 

-- HELPERS
everyf :: Int -> [a] -> [a]
everyf n [] = []
everyf n as = head as : everyf n (drop n as)
every n k = everyf n . drop (k-1)

teveryf :: (Eq a) => Int -> [a] -> [a]
teveryf n [] = []
teveryf n as = (take 3 as) ++ (teveryf n (drop n as))
tevery n k = teveryf n . drop (k - 1)

-- MODULE FUNCTIONS

sanitizeRow :: Region -> Region
sanitizeRow [] = []
sanitizeRow (r:rs) = if length r > 0 then [[0]] else r : sanitizeRow rs

extractRow :: Board -> Int -> Region
extractRow b i = take 9 $ drop ((i-1) * 9) $ b

extractCol :: Board -> Int -> Region
extractCol b i = every 9 i b

extractBox :: Board -> Int -> Region
extractBox b i = take 9 $ tevery 9 k b
    where k = [1,4,7,28,31,34,55,58,61] !! (i-1)

s_transpose :: Board -> Board
s_transpose b = helper b 1
    where helper b i
            | i > 9 = []
            | otherwise = (extractCol b i) ++ (helper b (i + 1))

s_btransform :: Board -> Board
s_btransform b = helper b 1
    where helper b i
             | i > 9 = []
             | otherwise = (extractBox b i) ++ (helper b (i + 1))

sweep :: Region -> Region
sweep r =
    let singles = concat $ filter (null . tail) r
    in map (\x -> if length x > 1 then x \\ singles else x) r

usweep :: Region -> Region
usweep r
    | null new = r
    | otherwise = (fst b) ++ (filter (== head new) $ head $ snd b) : (tail $ snd b)
    where rots = init (zipWith (++) (tails r) (inits r))
          uniques = filter (not . null) $ [foldl (\\) (head lst) (tail lst) | lst <- rots]
          singles = filter (null . tail) r
          new = concat $ uniques \\ singles
          b = break (elem $ head new) r

ecsweep :: Region -> Region
ecsweep r = helper r start
    where start = 9 - (length $ filter (null . tail) r) - 1
          helper r s
              | s < 2 = r
              | otherwise = helper (ecsweepk r s) (s - 1)

ecsweepk :: Region -> Int -> Region
ecsweepk r k
    | null ks = r
    | otherwise = (fst b) ++ (filter (== head ks) $ head $ snd b) : (tail $ snd b)
    where ns = filter (not . null . tail) r
          ecs = filter ((==k) . length) $ map (foldl union []) $ filter ((==k) . length) $ subsequences ns
          ks = concat $ filter ((==1) . length) $ map (filter (\x -> not $ elem x $ concat ecs)) ns
          b = break (elem $ head ks) r

finished :: Board -> Bool
finished [] = True
finished (b:bs)
    | length b > 1 = False
    | otherwise = finished bs

checkRows :: Board -> Board
checkRows [] = []
checkRows b = ((usweep . sweep) r) ++ (checkRows rs)
    where r = take 9 b
          rs = drop 9 b

checkCols :: Board -> Board
checkCols = s_transpose . checkRows . s_transpose

checkBoxes :: Board -> Board
checkBoxes = s_btransform . checkRows . s_btransform

ecSweepRows :: Board -> Board
ecSweepRows [] = []
ecSweepRows b = (ecsweep r) ++ (ecSweepRows rs)
    where r = take 9 b
          rs = drop 9 b

ecSweepCols :: Board -> Board
ecSweepCols = s_transpose . ecSweepRows . s_transpose

ecSweepBoxes :: Board -> Board
ecSweepBoxes = s_btransform . ecSweepRows . s_btransform

solveBoardLvl2 :: (Board -> Board) -> (Board -> Bool) -> Board -> Board
solveBoardLvl2 f term board
    | (board == nextBoard) = board
    | otherwise = solveBoard f term nextBoard
    where nextBoard = ecSweepRows board

solveBoard :: (Board -> Board) -> (Board -> Bool) -> Board -> Board
solveBoard f term board
    | (board == nextBoard) = solveBoardLvl2 f term nextBoard
    | term nextBoard = nextBoard
    | otherwise = solveBoard f term nextBoard
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
  lines <- getLine
  let board = populateBoard initBoard lines
  let solution = solveBoard (checkRows . checkCols . checkBoxes) finished board
  let rows = map (concat . extractRow solution) [1..9]
  mapM_ (putStrLn . concatMap show) rows
  

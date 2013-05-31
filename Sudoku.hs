module Sudoku (solve, generate, sudokuMain) where

import Data.Char (digitToInt)
import System.Environment (getArgs)
import System.Exit
import Data.List
import System.Random

import qualified Yices.Easy as Y
import Yices.Easy.Sugar
import qualified Yices.Easy.Build as YB
import Control.Monad ( forM_, liftM2, when )
import qualified Data.Map as M

data Cell = Undecided [Int]
          | Decided Int
          deriving (Show, Eq)

type Region = [Cell]
type Board = [Cell] 

cellShow (Undecided _) = show 0
cellShow (Decided v) = show v

decided :: Cell -> Bool
decided (Decided _) = True
decided _ = False

fromCell :: Cell -> [Int]
fromCell (Undecided xs) = xs
fromCell (Decided x) = [x]

toCell :: [Int] -> Cell
toCell [] = Decided 33
toCell [x] = Decided x
toCell xs = Undecided xs

prune :: [Int] -> Cell -> Cell
prune ss (Undecided xs)
    | null ns = Decided 33
    | null (tail ns) = Decided (head ns)
    | otherwise = Undecided ns
    where
        ns = xs \\ ss
prune _ d@(Decided _) = d

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
    let singles = concatMap fromCell $ filter decided r
    in map (prune singles) r

repetition :: ([[Int]], [[Int]]) -> [Int] -> [[Int]]
repetition b new = (fst b) ++ (filter (== head new) $ head $ snd b) : (tail $ snd b)

usweep :: Region -> Region
usweep r
    | null new = r
    | otherwise = map toCell $ repetition b new
    where rots = init (zipWith (++) (tails r) (inits r))
          uniques = filter (not . null) $ [foldl (\\) (head lst) (tail lst) | lst <- map (map fromCell) rots]
          singles = filter decided r
          new = concat $ uniques \\ map fromCell singles
          b = break (elem $ head new) $ map fromCell r

ecsweep :: Region -> Region
ecsweep r = helper r start
    where start = 9 - (length $ filter decided r) - 1
          helper r s
              | s < 2 = r
              | otherwise = helper (ecsweepk r s) (s - 1)

ecsweepk :: Region -> Int -> Region
ecsweepk r k
    | null ecs = r
    | otherwise = map toCell $ [if (x \\ (head ecs) == []) then x else (x \\ (head ecs)) | x <- riq]
    where riq = map fromCell r
          ns = filter (not . decided) r
          ecs = filter ((==k) . length) $ map (foldl union []) $ filter ((==k) . length) $ subsequences $ map fromCell ns

finished :: Board -> Bool
finished [] = True
finished (b:bs)
    | not (decided b) = False
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

-- really not too different from solveBoard. might be a way to DRY the code off here.
-- this function takes a board and returns false if there is a contradiction
findContradiction :: [(Board -> Board)] -> (Board -> Bool) -> Int -> Board -> Either Bool Board
findContradiction flist@(f:fs) term round board
    | ((>0) . length) $ filter (==Decided 33) nextBoard = Left False
    | term nextBoard = Right nextBoard
    | round == 10 = Right board
    | (board == nextBoard && round == 3) = findContradiction (ecSweepRows : ecSweepCols : ecSweepBoxes : fs) term (round+1) nextBoard
    | (board == nextBoard) = findContradiction fs term (round+1) nextBoard
    | otherwise = findContradiction fs term 1 nextBoard
    where nextBoard = f board

guess :: Board -> Board
guess b = takeWhile decided b ++ [toCell [head $ fromCell $ head $ dropWhile decided b]] ++ (tail $ dropWhile decided b)

reduce :: Board -> Board
reduce b = takeWhile decided b ++ [toCell $ tail $ fromCell $ head $ dropWhile decided b] ++ (tail $ dropWhile decided b)

bruteForce :: Board -> Board
bruteForce board =  
  let reducedBoard = reduce board
      guessBoard = guess board
      contraBoard = findContradiction (cycle [checkRows, checkCols, checkBoxes]) finished 1 guessBoard
  in case contraBoard of
    Left bool -> bruteForce reducedBoard
    Right b -> b                     

-- takes a list of functions that operate on boards, a predicate to decide
-- if we are done, a round counter to know how deep we are, and the current
-- board. from this it solves and returns a board
solveBoard :: [(Board -> Board)] -> (Board -> Bool) -> Int -> Board -> Board
solveBoard flist@(f:fs) term round board
    | term nextBoard = nextBoard
    | round == 10 = bruteForce board
    | (board == nextBoard && round == 3) = solveBoard (ecSweepRows : ecSweepCols : ecSweepBoxes : fs) term (round+1) nextBoard
    | (board == nextBoard) = solveBoard fs term (round+1) nextBoard
    | otherwise = solveBoard fs term 1 nextBoard
    where nextBoard = f board

initBoard :: Board
initBoard = take 81 $ repeat $ Undecided [1..9]

populateBoard :: Board -> String -> Board
populateBoard [] _ = []
populateBoard _ [] = []
populateBoard (b:bs) (s:ss)
    | cell > 0 = Decided (digitToInt s) : populateBoard bs ss 
    | otherwise = b : populateBoard bs ss
    where cell = digitToInt s

readNthLineOfFile :: String -> Int -> IO String
readNthLineOfFile filename line = do
  contents <- readFile filename
  return $ case drop (line - 1) $ lines contents of
    [] -> take 81 $ repeat '0'
    l:_ -> l

readRandomLineOfFile :: String -> IO String
readRandomLineOfFile filename = do
  gen <- newStdGen
  let ns = randoms gen :: [Int]
  readNthLineOfFile filename (head ns `mod` 245)

-- MAIN
sudokuMain :: IO ()
sudokuMain = getArgs >>= parse >>= putStr

parse ["solve"]    = getLine >>= (runYices . parseBoard) >>= putStrLn >> exit
parse ["solve", n] = (readNthLineOfFile "puzzles/puzzle-pool" $ read n) >>= (runYices . parseBoard) >>= putStrLn >> exit
parse ["generate"] = generate >>= putStrLn >> exit
parse ["help"]     = usage    >> exit
parse ["version"]  = version  >> exit
parse []           = usage    >> exit

usage    = putStrLn "Usage: runhaskell sudoku.hs [help|version|solve|generate]\n\nsolve: < in a puzzle\nsolve n: puzzle from puzzle-pool\n"
version  = putStrLn "Sudoku-Solver version 0.1"
exit     = exitWith ExitSuccess
die      = exitWith (ExitFailure 1)

solve :: String -> String
solve line =
  let board = populateBoard initBoard line
      solution = solveBoard (cycle [checkRows, checkCols, checkBoxes]) finished 1 board
      rows = map (extractRow solution) [1..9]
  in concat $ map (concatMap cellShow) rows

showBoard :: Board -> String
showBoard b = intercalate ['\n'] $ map (concatMap cellShow) rows
  where
    rows = map (extractRow b) [1..9]

generate :: IO String
generate = do
  line <- (readRandomLineOfFile "puzzles/puzzle-pool")
  let board = populateBoard initBoard line
      rows = map (extractRow board) [1..9]
  return $ concat $ map (concatMap cellShow) rows 
  

-- Yices based solver

parseBoard :: String -> Board
parseBoard = populateBoard initBoard

runYices :: Board -> IO String
runYices b = do
    Y.Sat model <- Y.solve (yicesSolve b)
    let soln c = case M.lookup (cell c) model of Just (Y.ValInt k) -> k
        row i = map soln [(i, y) | y <- [0..8]]
    return $ intercalate ['\n'] [concatMap show (row i) | i <- [0..8]]

yicesSolve :: Board -> Y.Query
yicesSolve b = YB.execBuild $ do
  let cells = liftM2 (,) [0..8] [0..8]
  forM_ (zip cells b) $ \(c, bc) -> do
    x <- YB.declInt $ cell c
    if (decided bc) then
        YB.assert (x ==. fromIntegral (head (fromCell bc)))
      else
        YB.assert ((x >=. 1) &&. (x <=. 9))
  forM_ cells $ \c@(x, y) -> do
    let notEq c1 = YB.assert (Y.Var (cell c) /=. Y.Var (cell c1))
        others = nub $ rowIndexes x y ++ colIndexes x y ++ boxIndexes x y
    mapM_ notEq others

cell :: (Int,Int) -> String
cell (x,y) = concat ["c", show x, "_", show y]

rowIndexes, colIndexes, boxIndexes   :: Int -> Int -> [(Int, Int)]
rowIndexes x y = [(x, c) | c <- [0..8], c /= y]
colIndexes x y = [(r, y) | r <- [0..8], r /= x]
boxIndexes x y = [(r, c) | r <- rs, c <- cs, not $ r == x && c == y]
  where
    rs = [st..st+2]
    cs = [cst..cst+2]
    st = truncate (fromIntegral x / 3) * 3
    cst = truncate (fromIntegral y / 3) * 3

prepBoard :: Board -> [(Int, Int, Cell)]
prepBoard b = [(i, j, possible c i j) | i <- [0..8], j <- [0..8], c <- b]
  where
    rows = [extractRow b i | i <- [1..9]] :: [[Cell]]
    cols = [extractCol b i | i <- [1..9]]
    boxes = [extractBox b i | i <- [1..9]]
    rowI i j = truncate (fromIntegral i / 3 + fromIntegral j / 3)
    possible d@(Decided _) _ _ = d
    possible (Undecided _) i j = Undecided $ (([1..9] \\ decidedValues (rows !! i))
                                                      \\ decidedValues (cols !! j))
                                                      \\ decidedValues (boxes !! rowI i j)

decidedValues :: [Cell] -> [Int]
decidedValues = foldr extract []
  where
    extract (Decided x) xs = x:xs
    extract (Undecided _) xs = xs

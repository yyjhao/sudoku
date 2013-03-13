import qualified Data.Array.IArray as A
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Char
import System.Environment 

import Control.Parallel.Strategies
import Control.Parallel

data Cell = Value Int | Choices [Int] Int deriving Eq

type InBoard = A.Array (Int, Int) Cell

instance Show Cell where
    show (Value x) = show x
    show (Choices x _) = show x

toList :: (A.Array (Int, Int) a) -> [[a]]
toList grid = [[grid A.! (y, x) | x<-[lowx..highx]] |  y<-[lowy..highy]] 
    where ((lowx, lowy), (highx, highy)) =  A.bounds grid

show' :: Show a => [[a]] -> String
show' list = unlines $ map (unwords . (map show)) list

data Board = Trying InBoard (S.Set ((Int, Int), (Int, Int)))|
             Done (A.Array (Int, Int) Int) |
             Invalid deriving Eq

instance Show Board where
    show Invalid = "Invalid Board"
    show (Done x) = show' $ toList x
    show (Trying x _) = show' $ toList x

toDone :: Board -> Board
toDone (Trying v _) = Done $ A.amap getVal v

getVal :: Cell -> Int
getVal (Value x) = x

isValue (Value _) = True
isValue _ = False

numChoices = length . getChoices

ordering cell = (numChoices cell, degree cell)

noChoice (Choices [] _) = True
noChoice _ = False

getChoices (Choices x _) = x
degree (Choices _ x) = x

decreDegree (Choices x y) = Choices x (y + 1)

removeChoice x (Choices xs y) = Choices (delete x xs) y
newChoices ys (Choices xs d) = Choices ys d

keepWhile :: (a -> Bool) -> (a -> a) -> a -> a
keepWhile test act ini
    | test ini = keepWhile test act (act ini)
    | otherwise = ini

setTakeWhile :: (a -> Bool) -> (S.Set a) -> [a]
setTakeWhile f xs
    | S.null xs = []
    | f (S.findMin xs) = (S.findMin xs) : (setTakeWhile f (S.deleteMin xs))
    | otherwise = []

diffVal :: Int -> Cell -> Bool
diffVal val (Value x) = val /= x
diffVal val (Choices _ _) = True

gridFor :: Int -> Int -> [(Int, Int)]
gridFor r c = [(x, y) | x <- [sr..sr + 2], y <- [sc..sc + 2]]
    where
        sr = r `quot` 3 * 3
        sc = c `quot` 3 * 3

cleanGridFor :: Int -> Int -> [(Int, Int)]
cleanGridFor r c = [(x, y) | x <- [sr..sr + 2], y <- [sc..sc + 2], x /= r, y /= c]
    where
        sr = r `quot` 3 * 3
        sc = c `quot` 3 * 3

valid :: InBoard -> Int -> Int -> Int -> Bool
valid board i j val =
    and $
        (map (\c->diffVal val $ board A.! (i, c)) [0..8]) ++
        (map (\r->diffVal val $ board A.! (r, j)) [0..8]) ++
        (map (\(r, c)->diffVal val $ board A.! (r, c)) (gridFor i j))

genBoard :: [[Int]] -> Board
genBoard input =
    if any noChoice $ A.elems board then Invalid
        else (Trying board pq)
    where
        board = A.array ((0, 0), (8, 8)) [((a, b), f a b) | a <- [0..8], b <- [0..8]]
        f i j
            | input !! i !! j == 0 = Choices (filter (valid board i j) [1..9]) (-(degree i j))
            | otherwise = Value $ input !! i !! j
        pq = S.fromList [(ordering $ board A.! (i, j), (i, j)) |
                i <- [0..8], j <- [0..8], not $ isValue $ board A.! (i, j)]
        degree r c =
            length $ filter (not . isValue . (board A.!)) $
                [(r, i) | i <- [0..8], i /= c] ++
                [(i, c) | i <- [0..8], i /= r] ++
                cleanGridFor r c

assign :: Board -> Int -> Int -> Int -> Board
assign (Trying board opq) r c val =
    if S.null pq then toDone (Trying newBoard pq)
        else if (fst (fst $ S.findMin pq)) == 0 then Invalid
            else Trying newBoard pq
    where
        newBoard = board A.// ( [((r, c), Value val)] ++ ((parMap rseq) updateInd inds) )
        pq = foldr updatePq (S.delete ((ordering $ board A.! (r, c)), (r, c)) opq) inds
        inds =
            filter (not . isValue . (board A.!)) $
                [(r, i) | i <- [0..8], i /= c] ++
                [(i, c) | i <- [0..8], i /= r] ++
                cleanGridFor r c
        updatePq p opq =
            S.insert ((ordering $ newBoard A.! p), p) $
                S.delete ((ordering $ board A.! p), p) opq
        updateInd p@(r, c) = (p, decreDegree (removeChoice val $ board A.! p))

solve :: Board -> [Board]
solve Invalid = []
solve x@(Done _) = [x]
solve b@(Trying board pq) =
    if (S.size pq == 0) then [toDone b]
        else tryPos $ snd $ S.findMin pq
    where
        tryPos p@(r, c) = concat $ map (solve . (assign b r c)) $ getChoices (board A.! p)

solveSingle :: Board -> Board
solveSingle Invalid = Invalid
solveSingle x@(Done _) = x
solveSingle b@(Trying board pq) =
    if (S.size pq == 0) then toDone b
        else if isNothing result then Invalid
            else fromJust result
    where
        result = tryPos $ snd $ S.findMin pq
        tryPos p@(r, c) = find (/=Invalid) $ (parMap rseq) (solveSingle . (assign b r c)) $ getChoices (board A.! p)

getPq (Trying _ pq) = pq

toNineNineGrid :: [a] -> [[a]]
toNineNineGrid xs = (parMap rseq) (\r-> (parMap rseq) (\c-> xs !! ((9 * r) + c)) [0..8]) [0..8]

main = do
    args <- getArgs
    let num = if null args then 20
        else read $ args !! 0 :: Int
    stringBoards <- getContents
    let sboards = map (\l-> map digitToInt l) $ lines stringBoards :: [[Int]]
    let conNSolve = solveSingle . genBoard . toNineNineGrid
    putStr $ unlines $ map show $ (parMap rseq) (conNSolve . (sboards !!)) [0..num]

{-
 - 2048.hs
 -
 - Copyright (C) 2016 mumu <https://github.com/mumuxme>
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}

module L2048 where

import Data.List
import Data.Monoid              ((<>))
import System.IO
import System.Console.ANSI
import System.Posix.IO          (fdRead, stdInput)
import System.Random            (randomRIO)
import System.Exit              (exitSuccess)
import Prelude           hiding (Left, Right)

data Move = Left | Right | Up | Down
type Board = [[Int]]
type Coordinate = (Int, Int)

-- new randow value produced from this list
vals = [2, 2, 4, 4, 4]
-- unit board size
sizeGird = 7
-- Board: rowBoard × colBoard gird
rowBoard = 5
colBoard = 5


l2048 :: IO ()
l2048 = do
    hSetBuffering stdin NoBuffering
    board <- addRandom initBoard
    loop board

loop :: Board -> IO ()
loop board
  | isMove board = do
      drawBoard board
      case is2048 board of
        True -> putStrLn "Congratulations!"
        False -> do
            tmpBoard <- renew board
            if board == tmpBoard
               then loop board
               else do newBoard <- addRandom tmpBoard
                       loop newBoard
  | otherwise = do
      drawBoard board
      putStrLn "Game over :("

renew :: Board -> IO Board
renew board = do
    mv <- captureMove
    let newBoard = move mv board
    return newBoard


initBoard :: Board
initBoard = replicate colBoard <$> replicate rowBoard 0

-- the coordinates of zeros
countZero :: Board -> [Coordinate]
countZero board = filter (\(row, col) -> (board!!row)!!col == 0) coordinates
    where coordinates = [(r, c) | r <- [0..rowBoard-1], c <- [0..colBoard-1]]

random :: [a] -> IO a
random xs = do
    i <- randomRIO (0, length xs - 1)
    return $ xs !! i

addRandom :: Board -> IO Board
addRandom board = do
    rpos <- random (countZero board)
    rval <- random vals
    return (update board rpos rval)
        where
            update (xs:xss) (row, col) val
              | row == 0 = (replaceNth col val xs) : xss
              | otherwise = xs : (update xss (row-1, col) val)
            replaceNth n val (x:xs)
              | n == 0 = val : xs
              | otherwise = x : (replaceNth (n-1) val xs)


-- merge one row (to left)
merge :: [Int] -> [Int]
merge row = eles ++ zeros
    where eles = merged $ filter (/=0) row
          merged (x:y:xs)
            | (x == y) = (x + y) : merged xs
            | otherwise = x : merged (y:xs)
          merged xs = xs
          zeros = replicate (length row - length eles) 0

move :: Move -> Board -> Board
move Left = map merge
move Right = map (reverse . merge . reverse)
move Up = transpose . move Left  . transpose
move Down = transpose . move Right . transpose

isMove :: Board -> Bool
isMove board = not . null $ zeros
    where zeros = [Left, Right, Up, Down] >>= countZero . (`move` board)

is2048 :: Board -> Bool
is2048 board = not . null $ filter (== 2048) (concat board)

score :: Board -> String
score board = show . sum $ map (\x -> x*(f x)) (concat board)
    where f x
            | x < 4 = 0
            | x == 4 = 0
            | otherwise = 1 + f (x `div` 2)


drawBoard :: Board -> IO ()
drawBoard board = do
    resetScreen
    let nowSocre = score board
    putStr $ "2048"
            <> concat (replicate (4*sizeGird - 4 - 7 - length nowSocre) " ")
            <> "Score: "
            <> nowSocre
            <> "\n\n"
    mapM_ (putStr . putRow) board
    putStrLn ("\nMove: \"wasd\" or \"hjkl\" or \"↑↓←→\"\n" ++ "Exit: q")

resetScreen :: IO ()
resetScreen = clearScreen >> setSGR [Reset] >> setCursorPosition 0 0

-- TODO, <https://en.wikipedia.org/wiki/ANSI_escape_code>
getColor :: Int -> String
getColor value =
    case value of
      0     -> "\^[[100m"           -- Vivid Black
      2     -> "\^[[41m"            -- Dull Red
      4     -> "\^[[42m"            -- Dull Green
      8     -> "\^[[43m"            -- Dull Yellow
      16    -> "\^[[44m"            -- Dull Blue
      32    -> "\^[[45m"            -- Dull Magenta
      64    -> "\^[[46m"            -- Dull Cyan
      128   -> "\^[[101m"           -- Vivid Red
      256   -> "\^[[102m"           -- Vivid Grean
      512   -> "\^[[103m"           -- Vivid Yellow
      1024  -> "\^[[104m"           -- Vivid Blue
      2048  -> "\^[[105m"           -- Vivid Magenta
      _     -> ""

getVal :: Int -> String
getVal x = case x of
             0 -> bg1 ++ "*" ++ bg2
             x -> bg1 ++ show x ++ bg2
    where bg1 = concat (replicate (len `div` 2) " ")
          bg2 = concat (replicate (len - (len`div`2)) " ")
          len = (sizeGird - (length . show $ x))

putRow :: [Int] -> String
putRow row = rowTop ++ "\n" ++ rowMid ++ "\n" ++ rowBot ++ "\n"
    where rowBot = rowTop
          rowTop = concat $ map (\x -> (getColor x) ++ concat (replicate sizeGird " ") ++ "\^[[0m") row
          rowMid = concat $ map (\x -> (getColor x) ++ getVal x ++ "\^[[0m") row


captureMove :: IO Move
captureMove = do
    (str, bytes)  <- fdRead stdInput 3
    case lookup str keyMoves of
      Just x -> return x
      Nothing -> if str == "q" then exitSuccess else captureMove

keyMoves :: [(String, Move)]
keyMoves = keys [["a","d","w","s"], ["h","l","k","j"], ["\^[[D","\^[[C","\^[[A","\^[[B"]]
    where keys xss = concatMap (flip zip [Left, Right, Up, Down]) xss

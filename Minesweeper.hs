{-# LANGUAGE OverloadedStrings #-}

module Minesweeper where

import System.Random
import Math.Combinat.Permutations -- cabal install combinat
import Data.Char
import System.Console.ANSI -- cabal install ansi-terminal
import Data.Colour.SRGB

{-
Instructions for use:

To start a new game, simply run main.

How to Play:

The user will be presented a 9x9 grid with row IDs along the left side
and column IDs along the top. Hidden somewhere on the board are ten
mines. The user will be prompted to either flag a cell or reveal a
cell. If the user reveals a cell, then if the cell is not a mine, it
will cascade to all adjacent cells until it reaches a cell with a
digit value. Any cells with a digit value represent the number of
mines in the adjacent eight cells. If the user suspects a cell
contains a mine, they may choose to flag it. To remove a flag, simply
flag the cell again. To win the game, all ten mines must be flagged.
-}

type Board = [[Char]]

data Done = Win | Lose | Continue
    deriving (Eq)

-- Takes a list of 81 Char and creates a 9 by 9 Board.
listToBoard :: [Char] -> Board
listToBoard [] = []
listToBoard (a : b : c : d : e : f : g : h : i : xs) = [a, b, c, d, e, f, g, h, i] : listToBoard xs
listToBoard _ = error "unable to build board: list must contain 81 items"

-- Applies formatting to and then prints a Board.
-- Original Minesweeper colors:
--     1 - Blue
--     2 - Green
--     3 - Red
--     4 - Purple
--     5 - Maroon
--     6 - Cyan
--     7 - Black
--     8 - Grey
printBoard :: Board -> IO ()
printBoard b = do
    printColorStr "  1 2 3 4 5 6 7 8 9" (sRGB 0 0 0) (sRGB 255 255 255)
    putChar '\n'
    let rs = ['A'..'I']
    format rs b
    where
        format :: [Char] -> Board -> IO ()
        format (r : rs) (xs : xss) = do
            printColorStr [r, ' '] (sRGB 0 0 0) (sRGB 255 255 255)
            setSGR [Reset]
            format' xs
            putChar '\n'
            format rs xss
        format _ _ = putStr ""
        format' :: [Char] -> IO ()
        format' (x : []) = colorize x
        format' (x : xs) = do
            colorize x
            printColorStr " " (sRGB 0 0 0) (sRGB 255 255 255)
            format' xs
        format' _ = putStr ""
        colorize :: Char -> IO ()
        colorize c = do
            let m = [('1', sRGB 0 0 255), ('2', sRGB 0 255 0), ('3', sRGB 255 0 0), ('4', sRGB 128 0 255),
                     ('5', sRGB 153 0 77), ('6', sRGB 0 255 255), ('7', sRGB 0 0 0), ('8', sRGB 128 128 128),
                     ('.', sRGB 0 0 0), ('_', sRGB 0 0 0), ('X', sRGB 255 0 0), ('F', sRGB 255 0 0)]
            let v = lookup c m
            printColorStr [c] v (sRGB 255 255 255)
        lookup :: Char -> [(Char, Colour Float)] -> Colour Float
        lookup c ((c', v) : cs)
            | c == c' = v
            | otherwise = lookup c cs
        lookup c _ = error $ "invalid char " ++ show c

-- Determines if the given set of coordinates is a valid position in a Board.
inBoard :: Int -> Int -> Bool
inBoard i j = i >= 0 && i <= 8 && j >= 0 && j <= 8

-- Shuffles a list.
shuffle :: [a] -> IO [a]
shuffle xs = do
  let n = length xs
  perm <- getStdRandom (randomPermutation n)
  return $ permuteList perm xs

-- Counts the number of occurrences of an item in a list.
count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

-- Takes an Int between 0 and 9 and returns the corresponding Char.
-- E.g., intToChar 7 => '7'
intToChar :: Int -> Char
intToChar i = if i >= 0 && i <= 9 then
                  chr $ i + 48
              else error "invalid input: must be Int in range 0-9"

-- Takes a Char between 'A' and 'I' and maps it to an Int between 1 and 9, respectively, or a Char between '1' and '9' and maps it to an Int between 1 and 9, respectively.
-- E.g., charToInt 'G' => 7; charToInt => '7' = 7
charToInt :: Char -> Int
charToInt c = if c `elem` ['A'..'I'] then
                  ord c - 64
              else if c `elem` ['1'..'9'] then
                  ord c - 48
              else error "invalid input: must be Char in range 'A'-'I' or '1'-'9'"

-- The initial Board the user sees; consists of all '.'.
view :: IO Board
view = return [['.' | _ <- [1..9]] | _ <- [1..9]]

-- The Board hidden behind the scenes which contains all the bombs and counts of adjacent bombs.
hidden :: IO Board
hidden = do
    b <- shuffle $ ['X' | _ <- [1..10]] ++ ['_' | _ <- [1..71]]
    return $ findBombs 0 0 (listToBoard b)
    where
        findBombs :: Int -> Int -> Board -> Board
        findBombs 8 8 b = setElem 8 (setElem 8 (bombCount 8 8 b) (b !! 8)) b
        findBombs i j b = if b !! i !! j == '_' then
                                let b' = setElem i (setElem j (bombCount i j b) (b !! i)) b in
                                if j == 8 then
                                    findBombs (i+1) 0 b'
                                else findBombs i (j+1) b'
                            else if j == 8 then
                                findBombs (i+1) 0 b
                            else findBombs i (j+1) b
        bombCount :: Int -> Int -> Board -> Char
        bombCount i j b = let x = intToChar $ count 'X' (adjacent i j b) in if x == '0' then '_' else x
        adjacent :: Int -> Int -> Board -> [Char]
        adjacent i j b =
            (if inBoard (i-1) (j-1) then [b !! (i-1) !! (j-1)] else []) ++
            (if inBoard (i-1) (j) then [b !! (i-1) !! j] else []) ++
            (if inBoard (i-1) (j+1) then [b !! (i-1) !! (j+1)] else []) ++
            (if inBoard i (j-1) then [b !! i !! (j-1)] else []) ++
            (if inBoard i (j+1) then [b !! i !! (j+1)] else []) ++
            (if inBoard (i+1) (j-1) then [b !! (i+1) !! (j-1)] else []) ++
            (if inBoard (i+1) j then [b !! (i+1) !! j] else []) ++
            (if inBoard (i+1) (j+1) then [b !! (i+1) !! (j+1)] else [])

-- Modify an element in a list at the given index.
setElem :: Int -> a -> [a] -> [a]
setElem i _ [] = error $ "unable to set element at index " ++ show i ++ ": index out of bounds"
setElem 0 x' (x : xs) = x' : xs
setElem i x' a@(x : xs)
    | i < length a = x : setElem (i - 1) x' xs
    | otherwise = error $ "unable to set element at index " ++ show i ++ ": index out of bounds"

-- Takes a row and column from the user and reveals the corresponding cell(s) in the Board.
updateBoard :: Board -> Board -> IO (Board, Done)
updateBoard v h = do
    i <- getVal "Enter row (A-I): " (\i -> toUpper i `elem` ['A'..'I']) "Invalid row\n"
    j <- getVal "Enter column (1-9): " (\j -> j `elem` ['1'..'9']) "Invalid column\n"
    let x = v !! i !! j
    if x == 'F' then do
        putStr "That cell is flagged\n"
        return (v, Continue)
    else if x /= '.' then do
        putStr "That cell is already revealed\n"
        return (v, Continue)
    else do
        reveal i j v h []
    where
        reveal :: Int -> Int -> Board -> Board -> [(Int, Int)] -> IO (Board, Done)
        reveal i j v h m = do
            if (i, j) `elem` m || not (inBoard i j) then
                return (v, Continue)
            else do
                let x = v !! i !! j
                let y = h !! i !! j
                if x == 'F' || x `elem` ['1'..'9'] then
                    return (v, Continue)
                else if y `elem` ['1'..'9'] then
                    return $ (setElem i (setElem j y (v !! i)) v, Continue)
                else if y == 'X' then
                    return (h, Lose)
                else do
                    let v' = setElem i (setElem j '_' (v !! i)) v
                    (v2, _) <- reveal (i-1) (j-1) v' h ((i, j) : m)
                    (v3, _) <- reveal (i-1) j v2 h ((i, j) : (i-1, j-1) : m)
                    (v4, _) <- reveal (i-1) (j+1) v3 h ((i, j) : (i-1, j-1) : (i-1, j) : m)
                    (v5, _) <- reveal i (j-1) v4 h ((i, j) : (i-1, j-1) : (i-1, j) : (i-1, j+1) : m)
                    (v6, _) <- reveal i (j+1) v5 h ((i, j) : (i-1, j-1) : (i-1, j) : (i-1, j+1) : (i, j-1) : m)
                    (v7, _) <- reveal (i+1) (j-1) v6 h ((i, j) : (i-1, j-1) : (i-1, j) : (i-1, j+1) : (i, j-1) : (i, j+1) : m)
                    (v8, _) <- reveal (i+1) j v7 h ((i, j) : (i-1, j-1) : (i-1, j) : (i-1, j+1) : (i, j-1) : (i, j+1) : (i+1, j-1) : m)
                    reveal (i+1) (j+1) v8 h ((i, j) : (i-1, j-1) : (i-1, j) : (i-1, j+1) : (i, j-1) : (i, j+1) : (i+1, j-1) : (i+1, j) : m)

-- Prints a string in the given color.
printColorStr :: String -> Colour Float -> Colour Float -> IO()
printColorStr s fc bc = do
    setSGR [SetRGBColor Foreground fc, SetRGBColor Background bc]
    putStr s
    setSGR [Reset]

-- Counts the number of correctly placed flags.
countFlags :: Board -> Board -> Int
countFlags v h = countFlags' 0 0 v h
    where
        countFlags' :: Int -> Int -> Board -> Board -> Int
        countFlags' 8 8 ((x : xs) : xss) ((y : ys) : yss)
            | x == 'F' && y == 'X' = 1
            | otherwise = 0
        countFlags' i j ((x : xs) : xss) ((y : ys) : yss)
            | x == 'F' && y == 'X' && j == 8 = 1 + countFlags' (i+1) 0 xss yss
            | x == 'F' && y == 'X' = 1 + countFlags' i (j+1) (xs : xss) (ys : yss)
            | j == 8 = countFlags' (i+1) 0 xss yss
            | otherwise = countFlags' i (j+1) (xs : xss) (ys : yss)
        countFlags' i j v h = error $ "unable to count flags: error occured at position (" ++ show i ++ ", " ++ show j ++ ")"

-- Gets a Char from the user and checks if it's valid according to the given function.
getAction :: String -> (Char -> Bool) -> String -> IO Char
getAction p f e = do
    putStr p
    x <- getChar
    putChar '\n'
    if not $ f x then do
        putStr e
        getAction p f e
    else return $ toUpper x

-- Gets a Char from the user and checks if it's valid according to the given fuction.
-- Note: This utilizes the charToInt function, which requires the input to be in the range '1'-'9' or 'A'-'I'.
getVal :: String -> (Char -> Bool) -> String -> IO Int
getVal p f e = do
    putStr p
    x <- getChar
    putChar '\n'
    if not $ f x then do
        putStr e
        getVal p f e
    else return $ (charToInt $ toUpper x) - 1

-- Generates a random board and plays a single game of Minesweeper.
game :: IO Board -> IO Board -> IO ()
game v h = do
    v' <- v
    h' <- h
    loop v' h' Continue
    where
        loop :: Board -> Board -> Done -> IO ()
        loop v h d = do
            if d == Win then do
                printBoard h
                putStr "\aCongratulations! You win!\n"
            else if d == Lose then do
                printBoard h
                putStr "\aYou lose. Better luck next time!\n"
            else do
                printBoard v
                a <- getAction "Enter 'F' for flag or 'R' for reveal: " (\x -> toUpper x `elem` ['F', 'R']) "Invalid action\n"
                if a == 'F' then do
                    i <- getVal "Enter row (A-I): " (\i -> toUpper i `elem` ['A'..'I']) "Invalid row\n"
                    j <- getVal "Enter column (1-9): " (\j -> j `elem` ['1'..'9']) "Invalid column\n"
                    let x = v !! i !! j
                    if x /= '.' && x /= 'F' then do
                        putStr "That cell is already revealed\n\n"
                        loop v h d
                    else if x == '.' then do
                        let v' = setElem i (setElem j 'F' (v !! i)) v
                        if countFlags v' h == 10 then do
                            loop v' h Win
                        else do
                            loop v' h d
                    else do
                        let v' = setElem i (setElem j '.' (v !! i)) v
                        loop v' h d
                else if countFlags v h /= 10 then do
                    (v', d') <- updateBoard v h
                    putChar '\n'
                    loop v' h d'
                else do
                    loop v h Win

main :: IO ()
main = do
    game view hidden

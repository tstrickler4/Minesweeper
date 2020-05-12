{-# LANGUAGE OverloadedStrings #-}

module Minesweeper where

import System.Random
import Math.Combinat.Permutations -- cabal install combinat
import Data.Char
import System.Console.ANSI -- cabal install ansi-terminal

type Board = [[Char]]

data Done = Win | Lose | Continue
    deriving (Eq)

-- Takes a list of 81 Char and creates a 9 by 9 Board.
listToBoard :: [Char] -> Board
listToBoard [] = []
listToBoard (a : b : c : d : e : f : g : h : i : xs) = [a, b, c, d, e, f, g, h, i] : listToBoard xs
listToBoard _ = error "unable to build board: list must contain 81 items"

-- Applies formatting to and then prints a Board.
printBoard :: IO Board -> IO ()
printBoard b = do
    xss <- b
    putStr "  1 2 3 4 5 6 7 8 9\n"
    let rs = ['A'..'I']
    format rs xss
    where
        format :: [Char] -> Board -> IO ()
        format (r : rs) (xs : xss) = do
            putStr [r, ' ']
            format' xs
            putChar '\n'
            format rs xss
        format _ _ = putStr ""
        format' :: [Char] -> IO ()
        format' (x : []) = colorize x
        format' (x : xs) = do
            colorize x
            putChar ' '
            format' xs
        format' _ = putStr ""
        colorize :: Char -> IO ()
        colorize c = do
            let m = [('1', Vivid, Blue), ('2', Vivid, Green), ('3', Vivid, Red), ('4', Dull, Magenta),
                     ('5', Dull, Red), ('6', Vivid, Cyan), ('7', Vivid, Yellow), ('8', Dull, White),
                     ('.', Vivid, White), ('_', Vivid, White), ('X', Vivid, White)]
            let (i, v) = lookup c m
            setSGR [SetColor Foreground i v]
            putChar c
            setSGR []
        lookup :: Char -> [(Char, ColorIntensity, Color)] -> (ColorIntensity, Color)
        lookup c ((c', i, v) : cs)
            | c == c' = (i, v)
            | otherwise = lookup c cs
        lookup c _ = error $ "invalid char " ++ show c

-- Determines if the given coordinates is a valid position in a Board.
inBoard :: Int -> Int -> Bool
inBoard i j = i >= 0 && i <= 8 && j >= 0 && j <= 8

-- Shuffles a list.
shuffle :: [a] -> IO [a]
shuffle xs = do
  let n = length xs
  perm <- getStdRandom (randomPermutation n)
  return $ permuteList perm xs

-- Counts the number of occurrences in a list.
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

-- Take a row and column from the user and reveal the corresponding cell(s) in the Board.
updateBoard :: IO Board -> IO Board -> IO (Board, Done)
updateBoard v h = do
    i <- getVal "Enter row (A-I): " (\i -> toUpper i `elem` ['A'..'I']) "Invalid row\n"
    j <- getVal "Enter column (1-9): " (\j -> j `elem` ['1'..'9']) "Invalid column\n"
    v' <- v
    h' <- h
    reveal i j v' h' []
    where
        getVal :: String -> (Char -> Bool) -> String -> IO Int
        getVal p f e = do
            putStr p
            x <- getChar
            putChar '\n'
            if not $ f x then do
                putStr e
                getVal p f e
            else return $ (charToInt $ toUpper x) - 1
        reveal :: Int -> Int -> Board -> Board -> [(Int, Int)] -> IO (Board, Done)
        reveal i j v h m = do
            if (i, j) `elem` m || not (inBoard i j) then
                return (v, Continue)
            else do
                let x = h !! i !! j
                if x `elem` ['1'..'9'] then
                    return $ (setElem i (setElem j x (v !! i)) v, Continue)
                else if x == 'X' then
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

-- Creates a duplicate of a Board. Intended for saving the state of hidden to avoid reshuffling.
once :: IO Board -> IO Board
once b = do
    b' <- b
    return [i | i <- b']

-- Prints a string in the given color.
colorStr :: String -> ColorIntensity -> Color -> IO()
colorStr s i c = do
    setSGR [SetColor Foreground i c]
    putStr s
    setSGR []

countFlags :: IO Board -> IO Int
countFlags b = do
    b' <- b
    return $ sum [if b' !! i !! j == 'F' then 1 else 0 | i <- [0..8], j <- [0..8]]

game :: IO Board -> IO Board -> IO ()
game v h = do
    let h' = once h
    printBoard h'
    -- (v', d) <- updateBoard v h'
    -- printBoard $ return v'
    printBoard h'
    -- loop v h Continue
    -- where
    --     loop :: IO Board -> IO Board -> Done -> IO ()
    --     loop v h d = do
    --         if d == Continue then do
    --             c <- countFlags v
    --             if c /= 10 then do
    --                 printBoard v
    --                 (v', d') <- updateBoard v h
    --                 loop (return v') h d'
    --             else do
    --                 loop v h Win
    --         else if d == Win then do
    --             printBoard h
    --             putStr "\aCongratulations! You win!\n"
    --         else do
    --             printBoard h
    --             putStr "\aYou lose. Better luck next time!\n"

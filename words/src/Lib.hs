module Lib
    ( outputGrid
    , formatGrid 
    , findWord
    , findWordInLine
    , findWords
    , skew
    , coordsGrid
    , gridWithCoords
    , zipOverGrid
    , zipOverGridWith
    , cell2char
    , Cell(Cell, Indent)
    , findWordInCellLinePrefix
    ) where

import Data.List (isInfixOf ,transpose)
import Data.Maybe (catMaybes, listToMaybe)

data Cell =  Cell (Integer, Integer) Char
            | Indent deriving (Eq, Ord, Show)
type Grid a = [[a]]
zipOverGrid :: Grid a -> Grid b -> Grid(a, b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

coordsGrid :: Grid (Integer, Integer)
coordsGrid = 
    let rows = map repeat [0..]
        cols = repeat [0..]
    in zipOverGrid rows cols

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords grid = zipOverGridWith Cell coordsGrid grid

outputGrid :: Grid Cell -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

mapOvergrid :: (a -> b) -> Grid a -> Grid b
mapOvergrid = map . map

formatGrid :: Grid Cell -> String
formatGrid = unlines . mapOvergrid cell2char

cell2char :: Cell -> Char
cell2char (Cell _ c) = c
cell2char Indent = '?'

getLines :: Grid Cell -> [[Cell]]
getLines grid = 
    let horizontal = grid
        veritcal = transpose grid
        diagonal1 = diagonalize grid
        diagonal2 = diagonalize (map reverse grid)
        lines = horizontal ++ veritcal ++ diagonal1 ++ diagonal2
    in lines ++ (map reverse lines)

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
    let lines = getLines grid
        foundWords = map (findWordInLine word) lines
    in listToMaybe $ catMaybes foundWords
        --found = or $ map (findWordInLine word) lines
    --in if found then Just word else Nothing

diagonalize :: Grid Cell -> Grid Cell
diagonalize = (transpose . skew)

skew :: Grid Cell -> Grid Cell
skew [] = []
skew (l:ls) = l : skew( map indent ls)
    where indent line = Indent : line

findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid words = 
    let foundWords = map (findWord grid) words
    in catMaybes foundWords

findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word line = 
    let found = findWordInCellLinePrefix [] word line
    in case found of
        Nothing -> findWordInLine word (tail line)
        cs@(Just _) -> cs

findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellLinePrefix acc (x:xs) (c:cs) | x == cell2char c
    = findWordInCellLinePrefix (c : acc) xs cs
findWordInCellLinePrefix acc [] _ = Just $ reverse acc
findWordInCellLinePrefix _ _ _ = Nothing
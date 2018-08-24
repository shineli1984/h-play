module Soduku (
  readGrid,
  showGrid,
  showGridWithPossibilities,
  pruneCells,
  pruneGrid',
  fixM,
  pruneGrid
) where

import Data.List.Split (chunksOf)
import Data.Char (isDigit, digitToInt)
import Data.List ((\\), transpose)
import Control.Monad ((>=>))

data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)

type Row = [Cell]
type Grid = [Row]

readGrid :: String -> Maybe Grid
readGrid s
  | length s == 81 = traverse (traverse readCell) . chunksOf 9 $ s
  | otherwise      = Nothing
  where
    readCell '.' = Just $ Possible [1..9]
    readCell c
      | isDigit c && c > '0' = Just . Fixed . digitToInt $ c
      | otherwise            = Nothing

showGridWith :: (Cell -> String) -> Grid -> String
showGridWith showCell = unlines . fmap (unwords . fmap showCell)

showGrid :: Grid -> String
showGrid = showGridWith showCell
  where
    showCell (Fixed n) = show n
    showCell _ = "."


showGridWithPossibilities :: Grid -> String
showGridWithPossibilities = showGridWith showCell
  where
    showCell (Fixed n) =  "          " ++ show n
    showCell (Possible ns) =
      "[" ++
      fmap (\x -> if (x :: Int) `elem` ns then head $ show x else ' ') [1..9]
      ++ "]"

pruneCells :: [Cell] -> Maybe [Cell]
pruneCells cells = traverse pruneCell cells
  where
    fixeds = [x | Fixed x <- cells]

    pruneCell (Possible xs) = case xs Data.List.\\ fixeds of
      []  -> Nothing
      [y] -> Just $ Fixed y
      ys  -> Just $ Possible ys
    pruneCell x = Just x

chunksOf3 = chunksOf 3

subGridsToRows :: Grid -> Grid
subGridsToRows = concatMap zipRows . chunksOf3
  where zipRows rows = let [r1, r2, r3] = fmap chunksOf3 rows
                       in zipWith3 (\a b c -> a ++ b ++ c) r1 r2 r3

pruneGrid' :: Grid -> Maybe Grid
pruneGrid' grid =
  traverse pruneCells grid
  >>= fmap transpose . traverse pruneCells . transpose
  >>= fmap subGridsToRows . traverse pruneCells . subGridsToRows

fixM :: (Monad m, Eq a) => (a -> m a) -> a -> m a
fixM f x = f x >>= \x' -> if x' == x then return x else fixM f x'

pruneGrid :: Grid -> Maybe Grid
pruneGrid = fixM pruneGrid'
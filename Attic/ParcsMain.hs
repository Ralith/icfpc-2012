{-# LANGUAGE RecordWildCards #-}
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid
import Control.Arrow
import Data.Tuple

data MineCell = MineWall
              | MineRock
              | MineLambda
              | MineLift
              | MineEarth
              | MineEmpty
              | MineRobot
              deriving (Show, Ord, Eq)

data RobotAction = RobotUp
                 | RobotLeft
                 | RobotRight
                 | RobotDown
                 | RobotWait
                 | RobotAbort

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = case splitAt n xs of
    (ys,rs) | length ys < n -> [ys]
            | otherwise     -> ys : splitEvery n rs

showMap :: MineMap -> String
showMap MineMap{..} =
    unlines . map (map toChar) . reverse . splitEvery mm_width_x . Map.elems
            $ mm_cells
  where
    toChar c = case c of
        MineWall   -> '#'
        MineEarth  -> '.'
        MineRock   -> '*'
        MineLambda -> '\\'
        MineLift   -> 'L'
        MineEmpty  -> ' '
        MineRobot  -> 'R'

parseMineMap :: String -> [[MineCell]]
parseMineMap input = map (map fromChar) (lines input)
  where
    input_lines = lines input
    (cells,metainfo) = second tail
                     $ splitAt (length input_lines - 4) input_lines

    fromChar c = case c of
        '#'  -> MineWall
        '.'  -> MineEarth
        '*'  -> MineRock
        '\\' -> MineLambda
        'L'  -> MineLift
        ' '  -> MineEmpty
        'R'  -> MineRobot
        _    -> error "fromChar: invalid character"

data MineMap = MineMap
    { mm_width_x, mm_width_y :: Int
    , mm_cells :: Map (Int,Int) MineCell
    , mm_index :: Map MineCell (Set (Int,Int))
    , mm_water_level :: Int
    , mm_flooding    :: Int
    , mm_waterproof  :: Int
    } deriving (Show, Eq)

insertMM :: (Int,Int) -> MineCell -> MineMap -> MineMap
insertMM pos cell mine_map = mine_map
    { mm_cells = Map.insert pos cell (mm_cells mine_map)
    , mm_index = Map.insertWith (<>) cell (Set.singleton pos) $ case existing of
        Nothing       -> mm_index mine_map
        Just old_cell -> Map.adjust (Set.delete pos) old_cell (mm_index mine_map)
    }
  where
    existing = Map.lookup pos (mm_cells mine_map)

convert :: [[MineCell]] -> MineMap
convert mine_map = MineMap
    { mm_width_x = width_x
    , mm_width_y = width_y
    , mm_cells   = Map.fromList indices
    , mm_index   = Map.fromListWith (<>) (map (swap . first Set.singleton) indices)
    }
  where width_x = length (head mine_map)
        width_y = length mine_map
        indices = zip [(y,x) | y <- reverse [1..width_y], x <- [1..width_x]]
                      (concat mine_map)

tick :: MineMap -> MineMap
tick mine_map@MineMap{..} = Set.foldr ((.) . handle_rock) id rocks mine_map
  where
    rocks = maybe Set.empty (snd . Set.split (2,0)) (Map.lookup MineRock mm_index)

    handle_rock (y,x) = case mm_cells Map.! (y-1,x) of

        MineEmpty -> insertMM (y-1,x) MineRock . insertMM (y,x) MineEmpty

        MineRock
            | Just MineEmpty <- Map.lookup (y,x+1) mm_cells
            , Just MineEmpty <- Map.lookup (y-1,x+1) mm_cells ->
                insertMM (y,x) MineEmpty . insertMM (y-1,x+1) MineRock

            | Just MineEmpty <- Map.lookup (y,x-1) mm_cells
            , Just MineEmpty <- Map.lookup (y-1,x-1) mm_cells ->
                insertMM (y,x) MineEmpty . insertMM (y-1,x-1) MineRock

        MineLambda
            | Just MineEmpty <- Map.lookup (y,x+1) mm_cells
            , Just MineEmpty <- Map.lookup (y-1,x+1) mm_cells ->
                insertMM (y,x) MineEmpty . insertMM (y-1,x+1) MineRock

        _otherwise -> id

isSafe :: (Int,Int) -> MineMap -> Bool
isSafe (y,x) MineMap{..}
    | Just MineRock <- Map.lookup (y+2,x) mm_cells
    = Just MineEmpty /= Map.lookup (y+1,x) mm_cells
    | otherwise = True

move :: MineMap -> (Int,Int)
move MineMap{..} = undefined

main :: IO ()
main = do
    input <- getContents
    let mine_map = convert (parseMineMap input)
    print (mm_index mine_map)

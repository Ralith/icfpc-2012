{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Bolder.World
    (World(..), Cell(..), Action(..), Direction(..), Location,
     oppositeDirection, applyMovement,
     worldSize, worldInBounds, worldCell, worldNearbyCell, worldIndices,
     worldToList,
     robotDrowned,
     parseWorld, makeWorldData, mutateWorld,
     cellEnterable, cellPushable, cellIsEmpty, cellFalls,
     isLambdaCell, worldTicksL, worldDataL, worldBeardStateL, worldRazorsL,
     Word8Image,
     encodeCell, decodeCell,
     allNeighborPaths, allNearbyPaths,
     floodWorld, exits, findPath, distance)
    where

import Prelude hiding (Either(..))

import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans
import Data.Array.Unboxed
import Data.Array.ST
import Data.Word
import Data.Maybe
import Data.Tuple
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.List
--import Data.DeriveTH
import Data.Lens.Common
import Data.Conduit
import Data.Graph.AStar
import Data.PSQueue (PSQ, Binding(..))
import qualified Data.PSQueue as Q

type Location = (Int, Int)

type Word8Image = UArray (Int, Int) Word8

data World =
  World {
      worldData :: UArray Location Word8,
      worldTicks :: Int,
      worldFloodingLevel :: Int,
      worldFloodingTicksPerLevel :: Int,
      worldFloodingTicks :: Int,
      worldDrowningDuration :: Int,
      worldDrowningTicks :: Int,
      worldLambdasCollected :: Int,
      worldLambdasRequired :: Int,
      worldTrampolines :: Map.Map Int Location,
      worldTargets :: Map.Map Int [Location],
      worldRobotPosition :: Location,
      worldLiftPosition :: Location,
      worldBeardRate :: Int,
      worldBeardState :: Int,
      worldRazors :: Int
    }
    deriving (Show, Eq)

--lenses
worldTicksL :: Lens World Int
worldTicksL = lens worldTicks (\x w -> w {worldTicks = x})

worldDataL :: Lens World Word8Image
worldDataL = lens worldData (\x w -> w {worldData = x})

worldBeardStateL :: Lens World Int
worldBeardStateL = lens worldBeardState (\x w -> w {worldBeardState = x})

worldRazorsL :: Lens World Int
worldRazorsL = lens worldRazors (\x w -> w {worldRazors = x})


data Cell
  = RobotCell
  | WallCell
  | RockCell Bool
  | LambdaCell
  | LambdaLiftCell Bool
  | EarthCell
  | EmptyCell
  | TrampolineCell Int
  | TargetCell Int
  | BeardCell
  | RazorCell
  | HigherOrderRockCell Bool
  deriving (Eq, Ord, Show)


isLambdaCell :: Cell -> Bool
isLambdaCell LambdaCell = True
isLambdaCell _          = False


data Action
  = MoveAction Direction
  | ShaveAction
  | WaitAction
  | AbortAction
  deriving (Eq, Ord, Show)


data Direction
  = Left
  | Right
  | Up
  | Down
  deriving (Eq, Ord, Show)


class Movement movement where
  applyMovement :: movement -> Location -> Location


instance Movement Direction where
  applyMovement Left  (x, y) = (x - 1, y)
  applyMovement Right (x, y) = (x + 1, y)
  applyMovement Up    (x, y) = (x, y + 1)
  applyMovement Down  (x, y) = (x, y - 1)


instance Movement [Direction] where
  applyMovement [] index = index
  applyMovement (direction:rest) index =
    applyMovement rest $ applyMovement direction index


oppositeDirection :: Direction -> Direction
oppositeDirection Left = Right
oppositeDirection Right = Left
oppositeDirection Up = Down
oppositeDirection Down = Up


encodeCell :: Cell -> Word8
encodeCell  RobotCell                  = 1
encodeCell  WallCell                   = 2
encodeCell (RockCell False)            = 3
encodeCell (RockCell True)             = 4
encodeCell  LambdaCell                 = 5
encodeCell (LambdaLiftCell False)      = 6
encodeCell (LambdaLiftCell True)       = 7
encodeCell  EarthCell                  = 8
encodeCell  EmptyCell                  = 9
encodeCell (TrampolineCell index)      = 10 + (toEnum index)
encodeCell (TargetCell index)          = 20 + (toEnum index)
encodeCell  BeardCell                  = 31
encodeCell  RazorCell                  = 32
encodeCell (HigherOrderRockCell False) = 33
encodeCell (HigherOrderRockCell True)  = 34


decodeCell :: Word8 -> Cell
decodeCell 1  = RobotCell
decodeCell 2  = WallCell
decodeCell 3  = RockCell            False
decodeCell 4  = RockCell            True
decodeCell 5  = LambdaCell
decodeCell 6  = LambdaLiftCell      False
decodeCell 7  = LambdaLiftCell      True
decodeCell 8  = EarthCell
decodeCell 9  = EmptyCell
decodeCell 31 = BeardCell
decodeCell 32 = RazorCell
decodeCell 33 = HigherOrderRockCell False
decodeCell 34 = HigherOrderRockCell True
decodeCell c
    | c >= 10
    , c <  20   = TrampolineCell $ (fromEnum c) - 10
    | c <  30   = TargetCell $ (fromEnum c) - 20
    | otherwise = error $ "decodeCell " ++ show c ++ " is not a valid option"


worldSize :: World -> (Int, Int)
worldSize world =
  let (_, size) = bounds $ worldData world
  in size


worldInBounds :: World -> Location -> Bool
worldInBounds world (columnIndex, rowIndex) =
  let (width, height) = worldSize world
  in (columnIndex >= 1)
     && (columnIndex <= width)
     && (rowIndex >= 1)
     && (rowIndex <= height)


worldCell :: World -> Location -> Maybe Cell
worldCell world index =
  if worldInBounds world index
    then Just $ decodeCell $ worldData world ! index
    else Nothing


worldNearbyCell
  :: (Movement movement) => World -> Location -> movement -> Maybe Cell
worldNearbyCell world index movement =
  worldCell world $ applyMovement movement index


worldIndices :: World -> [Location]
worldIndices world =
  let (width, height) = worldSize world
  in [(columnIndex, rowIndex) |
      rowIndex    <- [1 .. height],
      columnIndex <- [1 .. width ]]


worldToList :: World -> [(Location, Cell)]
worldToList world =
  map (\(index, encodedCell) -> (index, decodeCell encodedCell))
      (assocs $ worldData world)


robotDrowned :: World -> Bool
robotDrowned world = worldDrowningTicks world >= worldDrowningDuration world


parseWorld :: T.Text -> World
parseWorld text  =
  let lines = T.split (== '\n') text
      (_, bodyLines, headerLines) =
        foldl' (\(bodyDone, bodySoFar, headerSoFar) line ->
                   if bodyDone
                     then (True, bodySoFar, headerSoFar ++ [line])
                     else if T.null line
                            then (True, bodySoFar, headerSoFar)
                            else (False, bodySoFar ++ [line], headerSoFar))
               (False, [], [])
               lines
      width = foldl' (\soFar line -> max soFar (T.length line)) 1 bodyLines
      height = length bodyLines
      keys = Map.fromListWith (++)
               $ map (\line -> let (key, rest) = T.break (\c -> c == ' ') line
                                   value = T.tail rest
                               in (key, [value]))
                     headerLines
      associations =
       concat
       $ zipWith (\lineText rowIndex ->
                    zipWith (\cellCharacter columnIndex ->
                               ((columnIndex, height - rowIndex),
                                readCell cellCharacter))
                            (let lineChars = T.unpack lineText
                                 lineWidth = T.length lineText
                                 padding = take (width - lineWidth)
                                                (repeat ' ')
                             in lineChars ++ padding)
                            [1..])
                 bodyLines
                 [0..]
      lambdasRequired =
        foldl' (\total (_, cell) ->
                  if cellContainsLambda cell
                    then total + 1
                    else total)
               0
               associations
      (trampolines, targets) =
          foldl' (\prev@(trampmap, targmap) link -> fromMaybe prev $
                  do let [tramp, targ] = map (flip T.index 0) $ T.splitOn " targets " link
                         locations = map swap associations
                     trampId <- findIndex (== tramp) ['A'..'I']
                     trampLoc <- lookup (TrampolineCell trampId) locations
                     targId <- findIndex (== targ) ['1'..'9']
                     targLoc <- lookup (TargetCell targId) locations
                     return (Map.insert          trampId targLoc    trampmap,
                             Map.insertWith (++) targId  [trampLoc] targmap))
                 (Map.empty, Map.empty) $ fromMaybe [] $ Map.lookup "Trampoline" keys
      beardRate = maybe 25 (read . T.unpack. (!! 0)) $ Map.lookup "Growth" keys
  in World { worldData = makeWorldData (width, height) associations,
             worldTicks = 0,
             worldFloodingLevel =
                 maybe 0 (read . T.unpack. (!! 0)) $ Map.lookup "Water" keys,
             worldFloodingTicksPerLevel =
                 maybe 0 (read . T.unpack. (!! 0)) $ Map.lookup "Flooding" keys,
             worldFloodingTicks = 0,
             worldDrowningDuration =
                 maybe 10 (read . T.unpack. (!! 0)) $ Map.lookup "Waterproof" keys,
             worldDrowningTicks = 0,
             worldLambdasCollected = 0,
             worldLambdasRequired = lambdasRequired,
             worldTrampolines = trampolines,
             worldTargets = targets,
             worldRobotPosition = fromMaybe (1,1) $ fmap fst
                                  $ find ((== RobotCell) . snd) associations,
             worldLiftPosition = fromMaybe (1,1) $ fmap fst
                                 $ find ((\x -> x == (LambdaLiftCell False)
                                             || x == (LambdaLiftCell True)) . snd) associations,
             worldBeardRate = beardRate,
             worldBeardState = beardRate - 1,
             worldRazors =
                 maybe 0 (read . T.unpack . (!! 0)) $ Map.lookup "Razors" keys
           }


makeWorldData :: (Int, Int) -> [(Location, Cell)] -> UArray (Int, Int) Word8
makeWorldData (width, height) associations =
  array ((1, 1), (width, height))
        (map (\(index, cell) -> (index, encodeCell cell)) associations)


mutateWorld :: World -> [((Int, Int), Cell)] -> World
mutateWorld world mutations =
  world {
      worldData = worldData world
                  // map (\(index, cell) -> (index, encodeCell cell))
                         mutations
    }


readCell :: Char -> Cell
readCell 'R'  = RobotCell
readCell '#'  = WallCell
readCell '*'  = RockCell False
readCell '\\' = LambdaCell
readCell 'L'  = LambdaLiftCell False
readCell 'O'  = LambdaLiftCell True
readCell '.'  = EarthCell
readCell 'W'  = BeardCell
readCell '!'  = RazorCell
readCell ' '  = EmptyCell
readCell '@'  = HigherOrderRockCell False
readCell c = fromMaybe WallCell
             $       (findIndex (== c) ['A'..'I'] >>= return . TrampolineCell)
             `mplus` (findIndex (== c) ['1'..'9'] >>= return . TargetCell)


cellEnterable :: Cell -> Bool
cellEnterable LambdaCell              = True
cellEnterable EarthCell               = True
cellEnterable RazorCell               = True
cellEnterable (TrampolineCell _)      = True
cellEnterable (LambdaLiftCell True)   = True
cellEnterable RobotCell               = True
cellEnterable cell | cellIsEmpty cell = True
                   | otherwise        = False


cellPushable :: Cell -> Bool
cellPushable (RockCell _) = True
cellPushable (HigherOrderRockCell _) = True
cellPushable _ = False


cellIsEmpty :: Cell -> Bool
cellIsEmpty EmptyCell = True
cellIsEmpty _         = False


cellFalls :: Cell -> Bool
cellFalls (RockCell _) = True
cellFalls (HigherOrderRockCell _) = True
cellFalls _ = False


cellContainsLambda :: Cell -> Bool
cellContainsLambda LambdaCell = True
cellContainsLambda (HigherOrderRockCell _) = True
cellContainsLambda _ = False


floodWorld :: (Location -> Cell -> Bool) -> World -> Location -> Source (ST s) [Direction]
floodWorld pred world start =
    do traversed <- lift (newArray ((1,1), (worldSize world)) False :: ST s (STUArray s Location Bool))
       let helper path loc =
               case worldCell world loc of
                 Just cell -> do
                   beenHere <- lift $ readArray traversed loc
                   if beenHere || (not $ cellEnterable cell)
                   then return ()
                   else do
                     when (pred loc cell) $
                          yield (reverse path)
                     lift $ writeArray traversed loc True
                     forM_ [Left, Right, Up, Down]
                        (\dir -> helper (dir:path) $ applyMovement dir loc)
                 _ -> return ()
       helper [] start
       return ()


allNeighborPaths :: [[Direction]]
allNeighborPaths = [[Up], [Down], [Left], [Right]]

allNearbyPaths = allNeighborPaths ++ [[Up,   Left], [Up,   Right],
                                      [Down, Left], [Down, Right]]


exits :: World -> Location -> [Direction]
exits world location =
    filter (\path -> maybe False cellEnterable $ worldCell world $ applyMovement path location)
           (map head allNeighborPaths)


-- findPath :: (Location -> Bool) -> World -> Location -> Location -> Maybe [Direction]
-- findPath safepred world start dest = runST $ do
--   cameFrom <- newArray ((1,1), (worldSize world)) 0 :: ST s (STUArray s Location Word8)
--   closed <- newArray ((1,1), (worldSize world)) False :: ST s (STUArray s Location Bool)
--   let helper :: PSQ Location Int -> ST s (Maybe [Direction])
--       helper open =
--            case Q.findMin open of
--              Nothing -> return Nothing
--              Just (current :-> _) ->
--                  if current == dest
--                  then return $ Just $ reconstructPath current
--                  else return Nothing
--       reconstructPath point = do
--         dir <- readArray cameFrom point
--         if dir == 0
--         then return []
--         else let dir' = decodeDir dir
--              in (reconstructPath $ applyMovement dir' point) >>= return . ((oppositeDirection dir') :)
--   helper $ Q.singleton start 0


encodeDir Up = 1
encodeDir Down = 2
encodeDir Left = 3
encodeDir Right = 4

decodeDir 1 = Up
decodeDir 2 = Down
decodeDir 3 = Left
decodeDir 4 = Right


distance :: (Floating a, Integral a1) => (a1, a1) -> (a1, a1) -> a
distance l1 l2 = sqrt $ fromIntegral $ ((fst l2 - fst l1)^2 + (snd l2 - snd l1)^2)


------------------------------------------------------------------------------------------
{-
$(do
    let dataTypes = [''World         ,
                     ''Cell          ,
                     ''Action        ,
                     ''Direction      ]

        customTypes = []

    decs <- derives [makeIs, makeFrom] $ 
                filter (\x -> not $ any (x==) customTypes) dataTypes
    return $ decs)
-}

{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Bolder.World
    (World(..), Cell(..), Action(..), Direction(..), Location,
     oppositeDirection, applyMovement,
     worldSize, worldInBounds, worldCell, worldNearbyCell, worldIndices,
     worldToList,
     robotDrowned,
     parseWorld, makeWorldData, mutateWorld,
     cellEnterable, cellPushable, cellIsEmpty)
    where

import Prelude hiding (Either(..))

import Data.Array.Unboxed
import Data.Word
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.List

type Location = (Int, Int)

data World =
  World {
      worldData :: UArray (Int, Int) Word8,
      worldTicks :: Int,
      worldFloodingLevel :: Int,
      worldFloodingTicksPerLevel :: Int,
      worldFloodingTicks :: Int,
      worldDrowningDuration :: Int,
      worldDrowningTicks :: Int,
      worldLambdasCollected :: Int
    }


data Cell
  = RobotCell
  | WallCell
  | RockCell Bool
  | LambdaCell
  | LambdaLiftCell Bool
  | EarthCell
  | EmptyCell
  deriving (Eq, Ord, Show)

data Action
  = MoveAction Direction
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
  applyMovement :: movement -> (Int, Int) -> (Int, Int)


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
encodeCell RobotCell = 1
encodeCell WallCell = 2
encodeCell (RockCell False) = 3
encodeCell (RockCell True) = 4
encodeCell LambdaCell = 5
encodeCell (LambdaLiftCell False) = 6
encodeCell (LambdaLiftCell True) = 7
encodeCell EarthCell = 8
encodeCell EmptyCell = 9


decodeCell :: Word8 -> Cell
decodeCell 1 = RobotCell
decodeCell 2 = WallCell
decodeCell 3 = RockCell False
decodeCell 4 = RockCell True
decodeCell 5 = LambdaCell
decodeCell 6 = LambdaLiftCell False
decodeCell 7 = LambdaLiftCell True
decodeCell 8 = EarthCell
decodeCell 9 = EmptyCell


worldSize :: World -> Location
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
      rowIndex <- [1 .. height],
      columnIndex <- [1 .. width]]
-- This is something worth testing


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
      keys = Map.fromList
               $ map (\line -> let (key, rest) = T.break (\c -> c == ' ') line
                                   value = T.tail rest
                               in (key, value))
                     headerLines
      floodingLevel =
        maybe 0 (read . T.unpack) $ Map.lookup "Water" keys
      floodingTicksPerLevel =
        maybe 0 (read . T.unpack) $ Map.lookup "Flooding" keys
      floodingTicks = 0
      drowningDuration =
        maybe 10 (read . T.unpack) $ Map.lookup "Waterproof" keys
      drowningTicks = 0
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
  in World { worldData = makeWorldData (width, height) associations,
             worldTicks = 0,
             worldFloodingLevel = floodingLevel,
             worldFloodingTicksPerLevel = floodingTicksPerLevel,
             worldFloodingTicks = floodingTicks,
             worldDrowningDuration = drowningDuration,
             worldDrowningTicks = drowningTicks,
             worldLambdasCollected = 0
           }


makeWorldData :: (Int, Int) -> [((Int, Int), Cell)] -> UArray (Int, Int) Word8
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
readCell 'R' = RobotCell
readCell '#' = WallCell
readCell '*' = RockCell False
readCell '\\' = LambdaCell
readCell 'L' = LambdaLiftCell False
readCell 'O' = LambdaLiftCell True
readCell '.' = EarthCell
readCell ' ' = EmptyCell
readCell _ = WallCell


cellEnterable :: Cell -> Bool
cellEnterable LambdaCell = True
cellEnterable EarthCell = True
cellEnterable cell | cellIsEmpty cell = True
                   | otherwise = False


cellPushable :: Cell -> Bool
cellPushable (RockCell _) = True
cellPushable _ = False


cellIsEmpty :: Cell -> Bool
cellIsEmpty EmptyCell = True
cellIsEmpty _ = False

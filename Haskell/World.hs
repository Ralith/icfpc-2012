{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module World
    (World(..), Cell(..), Action(..), Direction(..),
     oppositeDirection, applyMovement,
     worldSize, worldInBounds, worldCell, worldNearbyCell, decodeCell,
     robotSubmerged, robotDrowned,
     readWorld, makeWorldData,
     cellEnterable, cellIsEmpty)
    where

import Prelude hiding (Either(..))

import Data.Array.Unboxed
import Data.Word
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.List
import Data.Conduit
import qualified Data.Conduit.Binary as C hiding (lines)
import qualified Data.Conduit.Text as C
import qualified Data.Conduit.List as C

data World =
  World {
      worldData :: UArray (Int, Int) Word8,
      worldTicks :: Int,
      worldFloodingLevel :: Int,
      worldFloodingTicksPerLevel :: Int,
      worldFloodingTicks :: Int,
      worldDrowningDuration :: Int,
      worldDrowningTicks :: Int
    }


data Cell
  = RobotCell
  | WallCell
  | RockCell Bool
  | LambdaCell
  | LambdaLiftCell Bool
  | EarthCell
  | EmptyCell
  deriving (Eq, Ord)

data Action
  = MoveAction Direction
  | WaitAction
  | AbortAction
  deriving (Eq, Ord)


data Direction
  = Left
  | Right
  | Up
  | Down
  deriving (Eq, Ord)


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


worldSize :: World -> (Int, Int)
worldSize world =
  let (_, size) = bounds $ worldData world
  in size


worldInBounds :: World -> (Int, Int) -> Bool
worldInBounds world (columnIndex, rowIndex) =
  let (width, height) = worldSize world
  in (columnIndex >= 0)
     && (columnIndex < width)
     && (rowIndex >= 0)
     && (rowIndex < height)


worldCell :: World -> (Int, Int) -> Maybe Cell
worldCell world index =
  if worldInBounds world index
    then Just $ decodeCell $ worldData world ! index
    else Nothing


worldNearbyCell
  :: (Movement movement) => World -> (Int, Int) -> movement -> Maybe Cell
worldNearbyCell world index movement =
  worldCell world $ applyMovement movement index

robotSubmerged :: World -> Bool
robotSubmerged world =
    let (width, _) = worldSize world
    in any (\cell ->
                case cell of
                  Just RobotCell -> True
                  _ -> False) $
    concatMap (\rowIndex ->
               map (\columnIndex -> worldCell world (columnIndex, rowIndex))
                   [0 .. width - 1])
              [0 .. worldFloodingLevel world - 1]


robotDrowned :: World -> Bool
robotDrowned world = worldDrowningTicks world >= worldDrowningDuration world

readWorld :: FilePath -> IO World
readWorld filePath = do
  lines <- runResourceT
             $ C.sourceFile filePath
             $= C.decode C.ascii
             $= C.lines
             $$ C.consume
  let (_, bodyLines, headerLines) =
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
                               ((columnIndex, (height-1) - rowIndex),
                                readCell cellCharacter))
                            (let lineChars = T.unpack lineText
                                 lineWidth = T.length lineText
                                 padding = take (width - lineWidth)
                                                (repeat ' ')
                             in lineChars ++ padding)
                            [0..])
                 bodyLines
                 [0..]
  return $ World {
               worldData = makeWorldData (width, height) associations,
               worldTicks = 0,
               worldFloodingLevel = floodingLevel,
               worldFloodingTicksPerLevel = floodingTicksPerLevel,
               worldFloodingTicks = floodingTicks,
               worldDrowningDuration = drowningDuration,
               worldDrowningTicks = drowningTicks
             }


makeWorldData :: (Int, Int) -> [((Int, Int), Cell)] -> UArray (Int, Int) Word8
makeWorldData size associations =
  array ((0, 0), size)
        (map (\(index, cell) -> (index, encodeCell cell)) associations)


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


cellIsEmpty :: Cell -> Bool
cellIsEmpty EmptyCell = True
cellIsEmpty _ = False

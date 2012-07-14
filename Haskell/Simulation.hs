module Simulation (StepResult(..), advanceWorld, fallPossible, cellEnterable) where

import Prelude hiding (Either(..))

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map as Map

import World


data Circumstance
  = FallingDown
  | FallingDownLeft
  | FallingDownRight
  deriving (Eq, Ord)


data StepResult = Step World | Win | Abort | LossDrowned | LossCrushed


advanceWorld :: World -> Action -> StepResult
advanceWorld world action =
  let size@(width, height) = worldSize world
      --
      allIndices = [(columnIndex, rowIndex) |
                    columnIndex <- [0 .. width - 1],
                    rowIndex <- [0 .. height - 1]]
      --This is something worth testing
      (liftOpen, robotPosition) =
          foldl' (\(noLambdas, robotPosition) index ->
                      case fromMaybe EmptyCell $ worldCell world index of
                        LambdaCell -> (False, robotPosition)
                        RobotCell -> (noLambdas, index)
                        _ -> (noLambdas, robotPosition))
          (True, (0, 0))
          allIndices
      world2 = advanceRobot world robotPosition action
      circumstances =
        Map.fromList
         $ mapMaybe (\index ->
                       let isEmpty path =
                             maybe False cellIsEmpty
                                   $ worldNearbyCell world2 index path
                       in fmap (\circumstance -> (index, circumstance))
                            $ case fromMaybe EmptyCell
                                     $ worldCell world2 index of
                                cell | cellFalls cell ->
                                  case () of
                                    () | isEmpty [Down] -> Just FallingDown
                                       | otherwise -> Nothing
                                       | otherwise -> Nothing)
                    allIndices
      world3 =
        advanceWater
          $ world2 {
                worldData = makeWorldData size
                              $ map (advanceCell world2) allIndices,
                worldTicks = 1 + worldTicks world2
              }
  in if robotDrowned world3
     then LossDrowned
     else Step world3


advanceRobot :: World -> (Int, Int) -> Action -> World
advanceRobot world robotPosition action =
  let prospectivePosition =
        case action of
          MoveAction direction -> applyMovement direction robotPosition
          _ -> robotPosition
      priorOccupant = fromMaybe WallCell $ worldCell world prospectivePosition
      effective = cellEnterable priorOccupant
  in if effective
       then mutateWorld world
                        [(robotPosition, EmptyCell),
                         (prospectivePosition, RobotCell)]
       else world


advanceWater :: World -> World
advanceWater world =
    let floodValue =
          if worldFloodingTicks world >= worldFloodingTicksPerLevel world - 1
            then 1
            else 0
    in world {
           worldFloodingLevel =
             if worldFloodingTicksPerLevel world /= 0
               then worldFloodingLevel world + floodValue
               else worldFloodingLevel world,
           worldFloodingTicks =
             (worldFloodingTicks world + 1) * (1 - floodValue),
           worldDrowningTicks =
             if robotSubmerged world
               then 1 + worldDrowningTicks world
               else 0
         }


advanceCell :: World -> (Int, Int) -> ((Int, Int), Cell)
advanceCell world index =
  (index,
   let cell = fromMaybe EmptyCell $ worldCell world index
   in if cellFalls cell
      then advanceFallCell world cell index
      else if cellIsEmpty cell
           then fromMaybe cell
                    $ foldl' (\soFar path ->
                              mplus soFar $
                                    case worldNearbyCell world index path of
                                      Just cellAbove
                                          | cellFalls cellAbove
                                          , fallPossible world
                                                (applyMovement path index) ->
                                              Just $ cellAfterFalling cellAbove
                                      _ -> Nothing)
                    Nothing [[Up], [Up, Left], [Up, Right]]
           else cell)

fallPossible :: World -> (Int,Int) -> Bool
fallPossible world index = any (== Just EmptyCell)
                         $ map (worldNearbyCell world index) possibleDirections
  where
    possibleDirections = case worldNearbyCell world index Down of
        Just LambdaCell -> [[Down,Right]]
        Just EmptyCell  -> [[Down]]
        Just RockCell{} -> [[Down],[Down,Right],[Down,Left]]
        _otherwise      -> []

advanceFallCell :: World -> Cell -> (Int, Int) -> Cell
advanceFallCell world cell index =
  if fallPossible world index then EmptyCell else cellAtRest cell


cellFalls :: Cell -> Bool
cellFalls (RockCell _) = True
cellFalls _ = False


cellAtRest :: Cell -> Cell
cellAtRest (RockCell _) = RockCell False
cellAtRest cell = cell


cellAfterFalling :: Cell -> Cell
cellAfterFalling (RockCell _) = RockCell True
cellAfterFalling cell = cell

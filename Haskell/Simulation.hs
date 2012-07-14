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
      allIndices = worldIndices world
      (liftOpen, robotPosition) =
          foldl' (\(liftOpen, robotPosition) index ->
                      case fromMaybe EmptyCell $ worldCell world index of
                        LambdaCell -> (False, robotPosition)
                        RobotCell -> (liftOpen, index)
                        _ -> (liftOpen, robotPosition))
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
                                       | otherwise -> Nothing)
                    allIndices
      world3 =
        advanceWater
          $ world2 {
                worldData = makeWorldData size
                              $ map (advanceCell world2 liftOpen) allIndices,
                worldTicks = 1 + worldTicks world2
              }
      robotCrushed = Just (RockCell True) == worldNearbyCell world3 robotPosition Up
        || action == MoveAction Down
        && cellFalls (fromMaybe EmptyCell (worldNearbyCell world2 robotPosition Up))

  in if action == AbortAction
     then Abort
     else if robotDrowned world3
          then LossDrowned
          else if robotCrushed
               then LossCrushed
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
       then mutateWorld (world { worldLambdasCollected = worldLambdasCollected world +
                                                         if priorOccupant == LambdaCell
                                                         then 1
                                                         else 0})
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


advanceCell :: World -> Bool -> (Int, Int) -> ((Int, Int), Cell)
advanceCell world liftOpen index =
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
                                          , fallPossible world (Just path)
                                                (applyMovement path index) ->
                                              Just $ cellAfterFalling cellAbove
                                      _ -> Nothing)
                    Nothing [[Up], [Up, Left], [Up, Right]]
           else if (cell == LambdaLiftCell False) && liftOpen
                  then LambdaLiftCell True
                  else cell)

fallPossible :: World -> Maybe [Direction] -> (Int,Int) -> Bool
fallPossible world path index = case worldNearbyCell world index Down of
    Just LambdaCell
        | Just EmptyCell <- worldNearbyCell world index Right
        , Just EmptyCell <- worldNearbyCell world index [Down,Right]
        -> True

    Just EmptyCell
        | not (isJust path) || path == Just [Up]
        -> True

    Just RockCell{}
        | Just [Up,Right] <- path, rockMovesLeft, rockMovesRight
        -> False
        | not (isJust path) || path == Just [Up,Left], rockMovesRight -> True
        | not (isJust path) || path == Just [Up,Right], rockMovesLeft -> True

    _otherwise      -> False

  where
    rockMovesLeft
        | Just EmptyCell <- worldNearbyCell world index Left
        , Just EmptyCell <- worldNearbyCell world index [Down,Left]
        = True
        | otherwise = False

    rockMovesRight
        | Just EmptyCell <- worldNearbyCell world index Right
        , Just EmptyCell <- worldNearbyCell world index [Down,Right]
        = True
        | otherwise = False

advanceFallCell :: World -> Cell -> (Int, Int) -> Cell
advanceFallCell world cell index =
  if fallPossible world Nothing index then EmptyCell else cellAtRest cell


cellFalls :: Cell -> Bool
cellFalls (RockCell _) = True
cellFalls _ = False


cellAtRest :: Cell -> Cell
cellAtRest (RockCell _) = RockCell False
cellAtRest cell = cell


cellAfterFalling :: Cell -> Cell
cellAfterFalling (RockCell _) = RockCell True
cellAfterFalling cell = cell

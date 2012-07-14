module Bolder.Simulation (
    StepResult(..), 
    advanceWorld,
    advanceWorld', 
    advanceRobot,
    advanceWater,
    fallPossible, 
    cellEnterable,
    isLiftOpen,
    Circumstance(..)) where

import Prelude hiding (Either(..))

import Control.Monad
import Control.Monad.State
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Lens.Common

import Bolder.World


data Circumstance
  = FallingDown
  | FallingDownLeft
  | FallingDownRight
  deriving (Eq, Ord)


data StepResult
  = Step World
  | Win World
  | Abort World
  | LossDrowned World
  | LossCrushed World
  deriving (Show, Eq)

type Context = State World


isLiftOpen :: World -> [Location] -> Bool
isLiftOpen world = all (maybe True (not . isLambdaCell) . worldCell world)


getCircumstances ::  [Location] -> World -> Context (Map.Map Location Circumstance)             
getCircumstances indices world = 
    return $ Map.fromList $ mapMaybe (\index ->
                    let isEmpty path =
                          maybe False cellIsEmpty
                                $ worldNearbyCell world index path
                    in fmap (\circumstance -> (index, circumstance))
                         $ case fromMaybe EmptyCell
                                  $ worldCell world index of
                             cell | cellFalls cell ->
                               case () of
                                 () | isEmpty [Down] -> Just FallingDown
                                    | otherwise -> Nothing) 
                            indices        


getRobotPosition :: [Location] -> World -> Location                                    
getRobotPosition indices world  = 
    foldl' (\robotPosition index ->
           case fromMaybe EmptyCell $ worldCell world index of
             LambdaCell -> robotPosition
             RobotCell -> index
             _ -> robotPosition)
       (1, 1)
       indices 


isRobotCrushed :: Action -> World -> Location -> World -> Bool       
isRobotCrushed action oldWorld robotPosition newWorld  = 
    Just (RockCell True) == worldNearbyCell newWorld robotPosition Up
       || action == MoveAction Down
       && cellFalls (fromMaybe EmptyCell 
                (worldNearbyCell oldWorld robotPosition Up))          

incTicks :: Context ()
incTicks = modify (worldTicksL ^+= 1)

advanceWorld' :: World -> Action -> StepResult
advanceWorld' w a = evalState (advanceWorld a) w
--This can be cleaned up more with a State World
advanceWorld :: Action -> Context StepResult
advanceWorld action = do
  size@(width, height) <- gets worldSize
  allIndices           <- gets worldIndices 
  robotPosition        <- gets $ getRobotPosition allIndices
  modify (advanceRobot robotPosition action)
  
  liftOpen             <- gets $ flip isLiftOpen  allIndices
  circumstances        <- gets $ getCircumstances allIndices
  --save the old world
  oldWorld <- get
  let newWorldData = makeWorldData size $ 
                        map (advanceCell oldWorld liftOpen) allIndices
  --update the world data
  modify $ setL worldDataL newWorldData
  incTicks
        
  modify $ advanceWater (snd robotPosition)
          
  robotCrushed <- gets $ isRobotCrushed action oldWorld robotPosition
  world        <- get

  if action == AbortAction
     then return $ Abort world
     else if robotDrowned world
          then return $ LossDrowned world
          else if robotCrushed
               then return $ LossCrushed world
               else return $ Step world


advanceRobot :: Location -> Action -> World -> World
advanceRobot robotPosition action world =
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


advanceWater :: Int -> World -> World
advanceWater robotAltitude world =
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
             if robotAltitude <= worldFloodingLevel world
               then 1 + worldDrowningTicks world
               else 0
         }


advanceCell :: World -> Bool -> Location -> (Location, Cell)
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

fallPossible :: World -> Maybe [Direction] -> Location -> Bool
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

advanceFallCell :: World -> Cell -> Location -> Cell
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

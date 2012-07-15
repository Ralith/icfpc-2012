module Bolder.Simulation (
    StepResult(..), 
    advanceWorld,
    advanceWorld', 
    advanceRobot,
    advanceWater,
    fallPossible, 
    cellEnterable,
    isLiftOpen,
    worldPushable,
    points,
    allNeighborPaths,
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


getCircumstances ::  [Location] -> World -> (Map.Map Location Circumstance)             
getCircumstances indices world = 
     Map.fromList $ mapMaybe (\index ->
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


isRobotCrushed :: Action -> World -> World -> Bool       
isRobotCrushed action oldWorld newWorld  =
    let robotPosition = worldRobotPosition oldWorld
    in Just (RockCell True) == worldNearbyCell newWorld robotPosition Up
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
  robotPosition        <- gets worldRobotPosition
  beardState           <- gets worldBeardState
  beardRate            <- gets worldBeardRate
  razors               <- gets worldRazors
  modify (advanceRobot action)
  
  liftOpen             <- gets $ flip isLiftOpen  allIndices
  circumstances        <- gets $ getCircumstances allIndices
  --save the old world
  oldWorld <- get
  let newWorldData = makeWorldData size $ 
                        map (advanceCell oldWorld liftOpen action) allIndices
  --update the world data
  modify $ setL worldDataL newWorldData
  incTicks
  if beardState == 0
  then modify $ worldBeardStateL ^= beardRate - 1
  else modify $ worldBeardStateL ^-= 1

  when (action == ShaveAction && razors > 0)
       (modify $ worldRazorsL ^-= 1)
        
  modify $ advanceWater (snd robotPosition)
          
  robotCrushed <- gets $ isRobotCrushed action oldWorld
  world        <- get

  if action == AbortAction
     then return $ Abort world
     else if (worldRobotPosition world) == (worldLiftPosition world)
            then return $ Win world
            else if robotDrowned world
                  then return $ LossDrowned world
                  else if robotCrushed
                         then return $ LossCrushed world
                         else return $ Step world


advanceRobot :: Action -> World -> World
advanceRobot action world =
  let robotPosition = worldRobotPosition world
      prospectivePosition =
        case action of
          MoveAction direction -> applyMovement direction robotPosition
          _ -> robotPosition
      priorOccupant = fromMaybe WallCell $ worldCell world prospectivePosition
      effective = cellEnterable priorOccupant
                  || (case action of
                        MoveAction direction ->
                          cellPushable priorOccupant
                          && worldPushable world prospectivePosition direction
                        _ -> False)
  in if effective
       then case priorOccupant of
              LambdaCell ->
                  mutateWorld (world { worldLambdasCollected = worldLambdasCollected world + 1,
                                       worldRobotPosition    = prospectivePosition })
                              [(robotPosition, EmptyCell),
                               (prospectivePosition, RobotCell)]
              RazorCell ->
                  mutateWorld (world { worldRazors        = worldRazors world + 1,
                                       worldRobotPosition = prospectivePosition })
                              [(robotPosition, EmptyCell),
                               (prospectivePosition, RobotCell)]
              TrampolineCell id ->
                  let targetPosition = fromMaybe (error "Invalid target")
                                       $ Map.lookup id $ worldTrampolines world
                      Just (TargetCell targetId) = worldCell world targetPosition
                      targets = worldTargets world
                      trampLocs = fromMaybe [] $ Map.lookup targetId targets
                      trampLocToId loc = let Just (TrampolineCell id) = worldCell world loc
                                         in id
                  in mutateWorld (world {
                                    worldTrampolines = foldl' (flip Map.delete) (worldTrampolines world)
                                                       $ map trampLocToId trampLocs,
                                    worldTargets = Map.delete targetId targets,
                                    worldRobotPosition = targetPosition})
                               $ [(robotPosition, EmptyCell),
                                  (targetPosition, RobotCell)]
                               ++ map (flip (,) EmptyCell) trampLocs
              _ | cellPushable priorOccupant
                , MoveAction direction <- action ->
                 mutateWorld world { worldRobotPosition = prospectivePosition }
                   [(robotPosition, EmptyCell),
                    (prospectivePosition, RobotCell),
                    (applyMovement direction prospectivePosition,
                     priorOccupant)]
                | otherwise ->
                 mutateWorld world { worldRobotPosition = prospectivePosition }
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
             if robotAltitude < worldFloodingLevel world
               then 1 + worldDrowningTicks world
               else 0
         }


advanceCell :: World -> Bool -> Action -> Location -> (Location, Cell)
advanceCell world liftOpen action index =
  (index,
   let cell = fromMaybe EmptyCell $ worldCell world index
   in if cellFalls cell
      then advanceFallCell world cell index
      else if cellIsEmpty cell
           then if adjacentBeardGrows index world
                then BeardCell
                else fromMaybe cell
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
           else case cell of
                  LambdaLiftCell False | liftOpen -> LambdaLiftCell True
                  BeardCell | action == ShaveAction
                            , adjacentTo world index RobotCell
                            , worldRazors world > 0 -> EmptyCell
                  _ -> cell)

adjacentBeardGrows :: Location -> World -> Bool
adjacentBeardGrows location world =
    worldBeardState world == 0 && adjacentTo world location BeardCell

adjacentTo :: World -> Location -> Cell -> Bool
adjacentTo world location cell =
    any (== cell) (map ((fromMaybe WallCell) . worldNearbyCell world location) allNearbyPaths)

allNeighborPaths :: [[Direction]]
allNeighborPaths = [[Up], [Down], [Left], [Right]]

allNearbyPaths = allNeighborPaths ++ [[Up,   Left], [Up,   Right],
                                      [Down, Left], [Down, Right]]

fallPossible :: World -> Maybe [Direction] -> Location -> Bool
fallPossible world path index = case worldNearbyCell world index Down of
    Just LambdaCell
        | not (isJust path) || path == Just [Up,Left]
        , Just EmptyCell <- worldNearbyCell world index Right
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


worldPushable :: World -> Location -> Direction -> Bool
worldPushable world position direction
    | elem direction [Left, Right]
    , Just cell <- worldNearbyCell world position direction
    , cellIsEmpty cell = True
    | otherwise = False


points :: StepResult -> Int
points (Step w)        = (worldLambdasCollected w) * 25 - (worldTicks w)
points (Abort w)       = (worldLambdasCollected w) * 50 - (worldTicks w)
points (Win w)         = (worldLambdasCollected w) * 75 - (worldTicks w)
points (LossDrowned w) = points $ Step w
points (LossCrushed w) = points $ Step w

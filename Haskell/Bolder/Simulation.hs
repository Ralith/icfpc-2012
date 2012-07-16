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
    safeMove,
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
  = Step
  | Win
  | Abort
  | LossDrowned
  | LossCrushed
  deriving (Show, Eq)

type Context = State World


isLiftOpen :: World -> Bool
isLiftOpen world = worldLambdasCollected world >= worldLambdasRequired world


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


isRobotCrushed :: World -> World -> Bool
isRobotCrushed oldWorld newWorld  =
    isJust $ fallsInto oldWorld False
               $ applyMovement Up (worldRobotPosition newWorld)

incTicks :: Context ()
incTicks = modify (worldTicksL ^+= 1)

advanceWorld' :: World -> Action -> (StepResult, World)
advanceWorld' w a = evalState (advanceWorld a) w
--This can be cleaned up more with a State World
advanceWorld :: Action -> Context (StepResult, World)
advanceWorld action = do
  size@(width, height) <- gets worldSize
  allIndices           <- gets worldIndices 
  beardState           <- gets worldBeardState
  beardRate            <- gets worldBeardRate
  razors               <- gets worldRazors
  modify (advanceRobot action)
  modify advanceDrowning
  
  liftOpen             <- gets $ isLiftOpen
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
        
  modify advanceWater
          
  robotCrushed <- gets $ isRobotCrushed oldWorld
  world        <- get

  if action == AbortAction
     then return $ (Abort, world)
     else if (worldRobotPosition world) == (worldLiftPosition world)
            then return $ (Win, world)
            else if robotDrowned world
                  then return $ (LossDrowned, world)
                  else if robotCrushed
                         then return $ (LossCrushed, world)
                         else return $ (Step, world)


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


advanceDrowning :: World -> World
advanceDrowning world =
  let robotAltitude = snd $ worldRobotPosition world
  in world {
         worldDrowningTicks =
           if robotAltitude < worldFloodingLevel world
             then worldDrowningTicks world
             else 0
       }


advanceWater :: World -> World
advanceWater world =
    let robotAltitude = snd $ worldRobotPosition world
        floodValue =
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
               else worldDrowningTicks world
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
                else fromMaybe cell $ fallsInto world False index
           else case cell of
                  LambdaLiftCell False | liftOpen -> LambdaLiftCell True
                  BeardCell | action == ShaveAction
                            , adjacentTo world index RobotCell
                            , worldRazors world > 0 -> EmptyCell
                  _ -> cell)

fallsInto :: World -> Bool -> Location -> Maybe Cell
fallsInto world hypothesizeEmpty index =
    foldl' (\soFar path ->
                mplus soFar $
                      case worldNearbyCell world index path of
                        Just cellAbove
                            | cellFalls cellAbove
                            , fallPossible world (Just path)
                                               (applyMovement path index)
                                               (if hypothesizeEmpty
                                                then Just index
                                                else Nothing)
                                                   ->
                                                   Just $ cellAfterFalling world index cellAbove
                        _ -> Nothing)
    Nothing [[Up], [Up, Left], [Up, Right]]

safeMove :: World -> Location -> Location -> Bool
safeMove world prev next =
    if next == (applyMovement Down prev)
    then not $ isJust $ fallsInto world True prev
    else True

adjacentBeardGrows :: Location -> World -> Bool
adjacentBeardGrows location world =
    worldBeardState world == 0 && adjacentTo world location BeardCell

adjacentTo :: World -> Location -> Cell -> Bool
adjacentTo world location cell =
    any (== cell) (map ((fromMaybe WallCell) . worldNearbyCell world location) allNearbyPaths)

fallPossible :: World -> Maybe [Direction] -> Location -> Maybe Location -> Bool
fallPossible world path index hypothesizeEmpty =
  case (sampleCell $ applyMovement Down index) of
    Just LambdaCell
        | not (isJust path) || path == Just [Up,Left]
        , isEmpty Right, isEmpty [Down,Right]
        -> True

    Just EmptyCell
        | not (isJust path) || path == Just [Up]
        -> True

    Just RockCell{}
        | Just [Up,Right] <- path, rockMovesLeft, rockMovesRight
        -> False
        | not (isJust path) || path == Just [Up,Left], rockMovesRight -> True
        | not (isJust path) || path == Just [Up,Right], rockMovesLeft -> True
    
    Just HigherOrderRockCell{}
        | Just [Up,Right] <- path, rockMovesLeft, rockMovesRight
        -> False
        | not (isJust path) || path == Just [Up,Left], rockMovesRight -> True
        | not (isJust path) || path == Just [Up,Right], rockMovesLeft -> True

    _otherwise      -> False

  where
    rockMovesLeft
        | isEmpty Left, isEmpty [Down,Left] = True
        | otherwise = False

    rockMovesRight
        | isEmpty Right, isEmpty [Down,Right] = True
        | otherwise = False

    isEmpty :: Movement a => a -> Bool
    isEmpty path = maybe False cellIsEmpty $ sampleCell $ applyMovement path index

    sampleCell index =
        if (Just index == hypothesizeEmpty)
        then Just EmptyCell
        else case worldCell world index of
               Just RobotCell | isJust hypothesizeEmpty -> Just EmptyCell
               maybeCell -> maybeCell


advanceFallCell :: World -> Cell -> Location -> Cell
advanceFallCell world cell index =
  if fallPossible world Nothing index Nothing then EmptyCell else cellAtRest cell


cellAtRest :: Cell -> Cell
cellAtRest (RockCell _) = RockCell False
cellAtRest (HigherOrderRockCell False) = HigherOrderRockCell False
cellAtRest cell = cell


cellAfterFalling :: World -> Location -> Cell -> Cell
cellAfterFalling _ _ (RockCell _) = RockCell True
cellAfterFalling world loc (HigherOrderRockCell _) =
    if maybe True (not . cellIsEmpty) (worldNearbyCell world loc Down)
    then LambdaCell
    else HigherOrderRockCell True
cellAfterFalling _ _ cell = cell


worldPushable :: World -> Location -> Direction -> Bool
worldPushable world position direction
    | elem direction [Left, Right]
    , Just cell <- worldNearbyCell world position direction
    , cellIsEmpty cell = True
    | otherwise = False


points :: StepResult -> World -> Int
points Step w        = (worldLambdasCollected w) * 25 - (worldTicks w)
points Abort w       = (worldLambdasCollected w) * 50 - (worldTicks w)
points Win w         = (worldLambdasCollected w) * 75 - (worldTicks w)
points LossDrowned w = points Step w
points LossCrushed w = points Step w

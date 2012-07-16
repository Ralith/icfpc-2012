module Bolder.Planning (planner) where

import Prelude hiding (Either(..))

import Control.Monad
import Control.Monad.ST
import Control.Monad.Identity
import Data.Conduit
import qualified Data.Conduit.Binary as C hiding (lines)
import qualified Data.Conduit.Text as C
import qualified Data.Conduit.List as C
import Data.Array.Unboxed
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.State

import Bolder.Simulation
import Bolder.World


data Problem
  = RockInTheWayProblem Location
  | BeardInTheWayProblem Location
  deriving (Eq, Ord)


data Route =
  Route {
      routeActions :: [Action],
      routeProblems :: [Problem]
    }


planner :: (Monad m) => World -> Source m Action
planner world = do
  fromMaybe (yield AbortAction) $ do
    route <-        (nextRoute (goals (== LambdaCell) world) world)
            `mplus` (nextRoute (goals cellIsTrampoline world) world)
    return $ do
      (maybeWorld, _) <-
        foldM (\maybeWorldAndFlag action -> do
                 case maybeWorldAndFlag of
                   (Just world, True) -> do
                     yield action
                     case advanceWorld' world action of
                       (Step, newWorld)
                         | sufficientlySimilar world newWorld ->
                           return (Just newWorld, True)
                         | otherwise ->
                           return (Just newWorld, False)
                       _ -> return (Nothing, True)
                   _ -> return maybeWorldAndFlag)
              (Just world, True)
              route
      case maybeWorld of
        Nothing -> yield AbortAction
        Just world -> planner world


sufficientlySimilar :: World -> World -> Bool
sufficientlySimilar oldWorld newWorld = False


{-
nextRoute world = do
  route <- nextRoute' c world
  case routeProblems route of
    [] -> Just route
    (problem : rest) -> do
      -- candidateResolutions <- resolveProblem world problem
      world <- foldM (\world action -> do
                        case advanceWorld' world action of
                          Step newWorld -> Just newWorld
                          Win newWorld -> Just newWorld
                          Abort newWorld -> Just newWorld
                          _ -> Nothing)
                     world
                     (routeActions route)
      let allIndices = worldIndices world
          (liftOpen, maybeRobotPosition) =
            foldl' (\(liftOpen, maybeRobotPosition) index ->
                      case fromMaybe EmptyCell $ worldCell world index of
                        LambdaCell -> (False, maybeRobotPosition)
                        RobotCell -> (liftOpen, Just index)
                        _ -> (liftOpen, maybeRobotPosition))
                   (True, Nothing)
                   allIndices
      case maybeRobotPosition of
        Nothing -> Nothing
        Just robotPosition -> do
          next <- nextRoute world
          Just $ concatRoutes [resolution, next]
    _ -> Nothing
-}


nextRoute :: [Location] -> World -> Maybe [Action]
nextRoute candidates world =
  runIdentity $ paths candidates world
              $$ C.consume
                 >>= return
                     . fmap (\directions -> map MoveAction directions)
                     . foldl' (\maybeBest candidate ->
                                 case maybeBest of
                                   Nothing -> Just candidate
                                   Just best ->
                                     if length candidate < length best
                                       then Just candidate
                                       else maybeBest)
                              Nothing


goals :: (Cell -> Bool) -> World -> [Location]
goals pred w =
    --sortBy (\a b -> compare (distance origin a) (distance origin b)) $
    filter (\loc -> let cell = worldCell w loc
                    in (maybe False pred cell || cell == Just (LambdaLiftCell True))
                       && (if (maybe False cellEnterable cell)
                              && not (maybe False cellIsTrampoline cell)
                           then not $ (isJust $ fallsInto w True loc) && (exits w loc == [Down])
                           else True))
           $ worldIndices w

paths :: Monad m => [Location] -> World -> Source m [Direction]
paths candidates w =
      (C.sourceList candidates)
    $= C.mapMaybe (findPath (safeMove w) w (worldRobotPosition w))

{-
  fmap (\(route, _, _) -> route)
   $ foldl'
       (\maybeBestSoFar index ->
          let goalHere = case fromMaybe WallCell $ worldCell world index of
                           LambdaCell -> True
                           LambdaLiftCell True -> True
                           _ -> False
              candidate = easyRoute world index >>=
                          (\route ->
                             if routeIsSafe world route
                               then Just (route,
                                          routeLength world route,
                                          routeDifficulty world route)
                               else Nothing)
          in if goalHere
               then case maybeBestSoFar of
                      Nothing -> candidate
                      Just (bestRoute, bestDistance, bestDifficulty) ->
                        case candidate of
                          Nothing -> maybeBestSoFar
                          Just (route, distance, difficulty)
                            | difficulty < bestDifficulty -> candidate
                            | (difficulty == bestDifficulty)
                              && (distance < bestDistance) -> candidate
                            | otherwise -> maybeBestSoFar
               else maybeBestSoFar)
      Nothing
      (worldIndices world)
-}


resolveProblem :: World -> Problem -> [Route]
resolveProblem world (RockInTheWayProblem rockLocation) =
  let maybeRouteToBelow = easyRoute world (applyMovement Down rockLocation)
      maybeRouteToLeft = easyRoute world (applyMovement Left rockLocation)
      maybeRouteToRight = easyRoute world (applyMovement Right rockLocation)
  in catMaybes [do
                  route <- maybeRouteToBelow
                  return $ appendToRoute route Left,
                do
                  route <- maybeRouteToBelow
                  return $ appendToRoute route Right,
                do
                  route <- maybeRouteToLeft
                  return $ appendToRoute route Right,
                do
                  route <- maybeRouteToRight
                  return $ appendToRoute route Left]
resolveProblem world (BeardInTheWayProblem rockLocation) =
  catMaybes
    [do
       route <- easyRoute world (applyMovement Down rockLocation)
       return $ appendShaveToRoute route,
     do
       route <- easyRoute world (applyMovement Left rockLocation)
       return $ appendShaveToRoute route,
     do
       route <- easyRoute world (applyMovement Right rockLocation)
       return $ appendShaveToRoute route,
     do
       route <- easyRoute world (applyMovement Up rockLocation)
       return $ appendShaveToRoute route,
     do
       route <- easyRoute world (applyMovement [Down, Left] rockLocation)
       return $ appendShaveToRoute route,
     do
       route <- easyRoute world (applyMovement [Down, Right] rockLocation)
       return $ appendShaveToRoute route,
     do
       route <- easyRoute world (applyMovement [Up, Left] rockLocation)
       return $ appendShaveToRoute route,
     do
       route <- easyRoute world (applyMovement [Up, Right] rockLocation)
       return $ appendShaveToRoute route]


easyRoute :: World -> Location -> Maybe Route
easyRoute world endPosition =
  let loop :: Map Location Route -> Maybe Route
      loop routes =
        case Map.lookup endPosition routes of
          result@(Just _) -> result
          Nothing ->
            let (newRoutes, anyChanges) =
                  foldl' considerIndex
                         (routes, False)
                         (worldIndices world)
            in if anyChanges
              then loop newRoutes
              else Nothing
      considerIndex
        :: (Map Location Route, Bool) -> Location
        -> (Map Location Route, Bool)
      considerIndex (newRoutes, anyChanges) index =
        let noChange = (newRoutes, anyChanges)
            reachableBy directions =
              (Map.insert index directions newRoutes, True)
        in case Map.lookup index newRoutes of
             result@(Just _) -> noChange
             Nothing ->
               case fromMaybe WallCell $ worldCell world index of
                 cell | cellEnterable cell ->
                        case foldl' (considerDirection newRoutes index)
                                    Nothing
                                    [Left, Right, Down, Up] of
                          Nothing -> noChange
                          Just route -> reachableBy route
                      | Just problem <- cellProblemOfTraversing cell index ->
                        case foldl' (considerDirection newRoutes index)
                                    Nothing
                                    [Left, Right, Down, Up] of
                          Nothing -> noChange
                          Just route ->
                            reachableBy $ addProblemToRoute route problem
                      | otherwise -> noChange
      considerDirection
        :: Map Location Route -> Location
        -> Maybe Route -> Direction -> Maybe Route
      considerDirection newRoutes index maybeRoute direction =
        case maybeRoute of
          Just _ -> maybeRoute
          Nothing ->
            let adjacentIndex = applyMovement direction index
            in case Map.lookup adjacentIndex newRoutes of
                 Nothing -> Nothing
                 Just route ->
                   Just $ appendToRoute route (oppositeDirection direction)
  in loop $ Map.fromList
          $ mapMaybe (\(index, cell) ->
                        if cell == RobotCell
                          then Just (index, emptyRoute)
                          else Nothing)
                     (worldToList world)


routeIsSafe :: World -> Route -> Bool
routeIsSafe world route =
  let actionsAreSafe world [] =
        not $ deadly world (worldRobotPosition world)
      actionsAreSafe world (action:rest) =
        if deadly world (worldRobotPosition world)
          then False
          else case advanceWorld' world action of
                 (Step, newWorld) -> actionsAreSafe newWorld rest
                 (Win, _) -> True
                 _ -> False
  in actionsAreSafe world (routeActions route)


deadly :: World -> Location -> Bool
deadly world position
    | robotDrowned world = True
    | Just RockCell{} <- worldNearbyCell world position Up
    , Just cellL <- worldNearbyCell world position Left
    , Just cellR <- worldNearbyCell world position Right
    , Just cellD <- worldNearbyCell world position Down
    = not (cellSafe cellL Left || cellSafe cellR Right || cellSafe cellD Down)
    | otherwise = False
  where
    cellSafe cell direction
        | cellPushable cell =
          worldPushable world (applyMovement direction position) direction
        | otherwise = cellEnterable cell


emptyRoute :: Route
emptyRoute =
  Route {
      routeActions = [],
      routeProblems = []
    }


appendToRoute :: Route -> Direction -> Route
appendToRoute route direction =
  route {
      routeActions = routeActions route ++ [MoveAction direction]
    }


appendWaitToRoute :: Route -> Route
appendWaitToRoute route =
  route {
      routeActions = routeActions route ++ [WaitAction]
    }


appendShaveToRoute :: Route -> Route
appendShaveToRoute route =
  route {
      routeActions = routeActions route ++ [ShaveAction]
    }


addProblemToRoute :: Route -> Problem -> Route
addProblemToRoute route problem =
  route {
      routeProblems = routeProblems route ++ [problem]
    }


concatRoutes :: [Route] -> Route
concatRoutes routes =
  Route {
      routeActions = concat $ map routeActions routes,
      routeProblems = concat $ map routeProblems routes
    }


cellProblemOfTraversing :: Cell -> Location -> Maybe Problem
cellProblemOfTraversing (RockCell _) location =
  Just (RockInTheWayProblem location)
cellProblemOfTraversing _ _ = Nothing


routeLength :: World -> Route -> Int
routeLength world route =
  length $ routeActions route


routeDifficulty :: World -> Route -> Int
routeDifficulty world route =
  if null $ routeProblems route
    then 0
    else 1

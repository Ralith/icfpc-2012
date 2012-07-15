module Bolder.Planning (planner) where

import Prelude hiding (Either(..))

import Control.Monad
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

import Debug.Trace

data Problem
  = RockInTheWayProblem Location
  deriving (Eq, Ord)


data Route =
  Route {
      routeDirections :: [Direction],
      routeProblems :: [Problem]
    }


planner :: (Monad m) => World -> Source m Action
planner world = do
  fromMaybe (yield AbortAction) $ do
    route <- nextRoute world
    return $ do
      maybeWorld <-
        foldM (\maybeWorld direction -> do
                 case maybeWorld of
                   Nothing -> return Nothing
                   Just world -> do
                     let action = MoveAction direction
                     yield action
                     case advanceWorld' world action of
                       Step newWorld -> return $ Just newWorld
                       _ -> return Nothing)
              (Just world)
              (routeDirections route)
      case maybeWorld of
        Nothing -> yield AbortAction
        Just world -> planner world


nextRoute :: World -> Maybe Route
nextRoute world = do
  route <- nextRoute' world
  case routeProblems route of
    [] -> Just route
    (problem : rest) -> do
      resolution <- resolveProblem world problem
      world <- foldM (\world direction -> do
                        let action = MoveAction direction
                        case advanceWorld' world action of
                          Step newWorld -> Just newWorld
                          Win newWorld -> Just newWorld
                          Abort newWorld -> Just newWorld
                          _ -> Nothing)
                     world
                     (routeDirections route)
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


nextRoute' :: World -> Maybe Route
nextRoute' world =
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


resolveProblem :: World -> Problem -> Maybe Route
resolveProblem world (RockInTheWayProblem rockLocation) = do
  route <- easyRoute world (applyMovement Down rockLocation)
  route <- return $ appendToRoute route Left
  return route


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
  let directionsAreSafe world [] =
        not $ deadly world (worldRobotPosition world)
      directionsAreSafe world (direction:rest) =
        if deadly world (worldRobotPosition world)
          then False
          else case advanceWorld' world $ MoveAction direction of
                 Step newWorld ->
                   directionsAreSafe newWorld rest
                 Win _ -> True
                 _ -> False
  in directionsAreSafe world (routeDirections route)


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
      routeDirections = [],
      routeProblems = []
    }


appendToRoute :: Route -> Direction -> Route
appendToRoute route direction =
  route {
      routeDirections = routeDirections route ++ [direction]
    }


addProblemToRoute :: Route -> Problem -> Route
addProblemToRoute route problem =
  route {
      routeProblems = routeProblems route ++ [problem]
    }


concatRoutes :: [Route] -> Route
concatRoutes routes =
  Route {
      routeDirections = concat $ map routeDirections routes,
      routeProblems = concat $ map routeProblems routes
    }


cellProblemOfTraversing :: Cell -> Location -> Maybe Problem
cellProblemOfTraversing (RockCell _) location =
  Just (RockInTheWayProblem location)
cellProblemOfTraversing _ _ = Nothing


routeLength :: World -> Route -> Int
routeLength world route =
  length $ routeDirections route


routeDifficulty :: World -> Route -> Int
routeDifficulty world route =
  if null $ routeProblems route
    then 0
    else 1

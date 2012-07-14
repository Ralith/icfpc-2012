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
  let allIndices = worldIndices world
      (liftOpen, maybeRobotPosition) =
        foldl' (\(liftOpen, maybeRobotPosition) index ->
                  case fromMaybe EmptyCell $ worldCell world index of
                    LambdaCell -> (False, maybeRobotPosition)
                    RobotCell -> (liftOpen, Just index)
                    _ -> (liftOpen, maybeRobotPosition))
               (True, Nothing)
               allIndices
  fromMaybe (yield AbortAction) $ do
    robotPosition <- maybeRobotPosition
    route <- nextRoute world liftOpen robotPosition
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


nextRoute :: World -> Bool -> Location -> Maybe Route
nextRoute world liftOpen startPosition =
  fmap (\(route, _, _) -> route)
   $ foldl'
       (\maybeBestSoFar index ->
          let goalHere = case fromMaybe WallCell $ worldCell world index of
                           LambdaCell -> True
                           LambdaLiftCell _ | liftOpen -> True
                           _ -> False
              candidate = easyRoute world startPosition index >>=
                          (\route ->
                             if routeIsSafe world startPosition route
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


easyRoute :: World -> Location -> Location -> Maybe Route
easyRoute world startPosition endPosition =
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


routeIsSafe :: World -> Location -> Route -> Bool
routeIsSafe world robotPosition route =
  let directionsAreSafe world robotPosition [] =
        not $ deadly world robotPosition
      directionsAreSafe world robotPosition (direction:rest) =
        if deadly world robotPosition
          then False
          else case advanceWorld' world $ MoveAction direction of
                 Step newWorld ->
                   directionsAreSafe newWorld
                                     (applyMovement direction robotPosition)
                                     rest
                 Win _ -> True
                 _ -> False
  in directionsAreSafe world robotPosition (routeDirections route)


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
            boulderMovable world position [direction,direction]
        | otherwise = cellEnterable cell


boulderMovable :: World -> (Int,Int) -> [Direction] -> Bool
boulderMovable world pos dir
    | Just cell <- worldNearbyCell world pos dir, cellIsEmpty cell = True
    | otherwise = False


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

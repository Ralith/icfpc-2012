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
              route
      case maybeWorld of
        Nothing -> yield AbortAction
        Just world -> planner world


nextRoute :: World -> Bool -> (Int, Int) -> Maybe [Direction]
nextRoute world liftOpen startPosition =
  fmap fst
   $ foldl'
       (\maybeBestSoFar index ->
          let goalHere = case fromMaybe WallCell $ worldCell world index of
                           LambdaCell -> True
                           LambdaLiftCell _ | liftOpen -> True
                           _ -> False
              candidate = easyRoute world startPosition index >>=
                          (\route ->
                             if routeIsSafe world startPosition route
                               then Just (route, length route)
                               else Nothing)
          in if goalHere
               then case maybeBestSoFar of
                      Nothing -> candidate
                      Just (bestRoute, bestDistance) ->
                        case candidate of
                          Nothing -> maybeBestSoFar
                          Just (route, distance)
                            | distance < bestDistance -> candidate
                            | otherwise -> maybeBestSoFar
               else maybeBestSoFar)
      Nothing
      (worldIndices world)


easyRoute :: World -> (Int, Int) -> (Int, Int) -> Maybe [Direction]
easyRoute world startPosition endPosition =
  let loop :: Map (Int, Int) [Direction] -> Maybe [Direction]
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
                      | otherwise -> noChange
      considerDirection newRoutes index maybeRoute direction =
        case maybeRoute of
          Just _ -> maybeRoute
          Nothing ->
            let adjacentIndex = applyMovement direction index
            in case Map.lookup adjacentIndex newRoutes of
                 Nothing -> Nothing
                 Just route -> Just $ route ++ [oppositeDirection direction]
  in loop $ Map.fromList
          $ mapMaybe (\(index, cell) ->
                        if cell == RobotCell
                          then Just (index, [])
                          else Nothing)
                     (worldToList world)


routeIsSafe :: World -> (Int, Int) -> [Direction] -> Bool
routeIsSafe world robotPosition [] =
  not $ deadly world robotPosition
routeIsSafe world robotPosition (direction:rest) =
  if deadly world robotPosition
    then False
    else case advanceWorld' world $ MoveAction direction of
           Step newWorld ->
             routeIsSafe world (applyMovement direction robotPosition) rest
           Win _ -> True
           _ -> False


deadly :: World -> (Int,Int) -> Bool
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

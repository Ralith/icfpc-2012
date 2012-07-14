module Planning(planner) where

import Prelude hiding (Either(..))

import Data.Conduit
import qualified Data.Conduit.Binary as C hiding (lines)
import qualified Data.Conduit.Text as C
import qualified Data.Conduit.List as C
import Data.Array.Unboxed
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)

import World


planner :: World -> Source (ResourceT IO) Action
planner world = do
  mapM_ (\_ -> yield $ MoveAction Up)
        [0 .. 100]
  return ()


nextGoal :: World -> (Int, Int) -> Maybe (Int, Int)
nextGoal world startPosition =
  fmap snd
   $ foldl'
       (\maybeBestSoFar index ->
          let goalHere = case fromMaybe WallCell $ worldCell world index of
                           LambdaCell -> True
                           LambdaLiftCell True -> True
                           _ -> False
              maybeDistance = fmap length $ easyRoute world startPosition index
          in if goalHere
               then case maybeBestSoFar of
                      Nothing -> Just (fromMaybe 0 maybeDistance, index)
                      Just (bestDistance, bestGoal) ->
                        case maybeDistance of
                          Nothing -> maybeBestSoFar
                          Just distance
                            | distance < bestDistance -> Just (distance, index)
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
                         (range $ bounds $ worldData world)
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
            case Map.lookup (applyMovement direction index) newRoutes of
              Nothing -> Nothing
              Just route -> Just $ route ++ [oppositeDirection direction]
  in loop $ Map.fromList
          $ mapMaybe (\(index, cell) ->
                        if cell == RobotCell
                          then Just (index, [])
                          else Nothing)
                     (worldToList world)

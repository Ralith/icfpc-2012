{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Main (main) where

import Prelude hiding (Either(..))

import Control.Monad.Trans
import Data.Array.Unboxed
import Data.Conduit
import qualified Data.Conduit.Binary as C hiding (lines)
import qualified Data.Conduit.Text as C
import qualified Data.Conduit.List as C
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe hiding (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import System.Environment
import System.Exit
import System.IO
import System.Posix.Unistd


data World =
  World {
      worldData :: UArray (Int, Int) Word8,
      worldTicks :: Int,
      worldFloodingLevel :: Int,
      worldFloodingTicksPerLevel :: Int,
      worldFloodingTicks :: Int,
      worldDrowningDuration :: Int,
      worldDrowningTicks :: Int
    }


data Cell
  = RobotCell
  | WallCell
  | RockCell Bool
  | LambdaCell
  | LambdaLiftCell Bool
  | EarthCell
  | EmptyCell
  deriving (Eq, Ord)


data Action
  = MoveAction Direction
  | WaitAction
  | AbortAction
  deriving (Eq, Ord)


data Direction
  = Left
  | Right
  | Up
  | Down
  deriving (Eq, Ord)


class Movement movement where
  applyMovement :: movement -> (Int, Int) -> (Int, Int)


instance Movement Direction where
  applyMovement Left  (x, y) = (x - 1, y)
  applyMovement Right (x, y) = (x + 1, y)
  applyMovement Up    (x, y) = (x, y - 1)
  applyMovement Down  (x, y) = (x, y + 1)


instance Movement [Direction] where
  applyMovement [] index = index
  applyMovement (direction:rest) index =
    applyMovement rest $ applyMovement direction index


data Circumstance
  = FallingDown
  | FallingDownLeft
  | FallingDownRight
  deriving (Eq, Ord)


data StepResult = Step World | Win | Abort | LossDrowned | LossCrushed


main :: IO ()
main = do
  parameters <- getArgs
  case parameters of
    [worldFilePath] -> do
      world <- readWorld worldFilePath
      visualize world []
      runResourceT $ planner world
                       $= slower
                       $$ C.foldM (\world action -> do
                                     let result = advanceWorld world action
                                     case result of
                                       Step world' -> lift $ visualize world' [] >> return world'
                                       Win -> lift $ putStrLn "Win" >> exitSuccess
                                       Abort -> lift $ putStrLn "Abort" >> exitSuccess
                                       LossDrowned -> lift $ putStrLn "Lost: Drowned" >> exitSuccess
                                       LossCrushed -> lift $ putStrLn "Lost: Crushed" >> exitSuccess
                                  )
                       world
      exitSuccess
    _ -> do
      putStrLn $ "Usage: bolder input.map"
      exitFailure


slower :: Conduit a (ResourceT IO) a
slower = do
  liftIO $ usleep 200000
  maybeItem <- await
  case maybeItem of
    Just item -> do
      yield item
      slower
    Nothing -> return ()




readWorld :: FilePath -> IO World
readWorld filePath = do
  lines <- runResourceT
             $ C.sourceFile filePath
             $= C.decode C.ascii
             $= C.lines
             $$ C.consume
  let (_, bodyLines, headerLines) =
        foldl' (\(bodyDone, bodySoFar, headerSoFar) line ->
                   if bodyDone
                     then (True, bodySoFar, headerSoFar ++ [line])
                     else if T.null line
                            then (True, bodySoFar, headerSoFar)
                            else (False, bodySoFar ++ [line], headerSoFar))
               (False, [], [])
               lines
      width = foldl' (\soFar line -> max soFar (T.length line)) 1 bodyLines
      height = length bodyLines
      keys = Map.fromList
               $ map (\line -> let (key, rest) = T.break (\c -> c == ' ') line
                                   value = T.tail rest
                               in (key, value))
                     headerLines
      floodingLevel =
        maybe 0 (read . T.unpack) $ Map.lookup "Water" keys
      floodingTicksPerLevel =
        maybe 0 (read . T.unpack) $ Map.lookup "Flooding" keys
      floodingTicks = 0
      drowningDuration =
        maybe 10 (read . T.unpack) $ Map.lookup "Waterproof" keys
      drowningTicks = 0
      associations =
       concat
       $ zipWith (\lineText rowIndex ->
                    zipWith (\cellCharacter columnIndex ->
                               ((columnIndex, (height-1) - rowIndex),
                                readCell cellCharacter))
                            (let lineChars = T.unpack lineText
                                 lineWidth = T.length lineText
                                 padding = take (width - lineWidth)
                                                (repeat ' ')
                             in lineChars ++ padding)
                            [0..])
                 bodyLines
                 [0..]
  return $ World {
               worldData = makeWorldData (width, height) associations,
               worldTicks = 0,
               worldFloodingLevel = floodingLevel,
               worldFloodingTicksPerLevel = floodingTicksPerLevel,
               worldFloodingTicks = floodingTicks,
               worldDrowningDuration = drowningDuration,
               worldDrowningTicks = drowningTicks
             }


makeWorldData :: (Int, Int) -> [((Int, Int), Cell)] -> UArray (Int, Int) Word8
makeWorldData size associations =
  array ((0, 0), size)
        (map (\(index, cell) -> (index, encodeCell cell)) associations)


readCell :: Char -> Cell
readCell 'R' = RobotCell
readCell '#' = WallCell
readCell '*' = RockCell False
readCell '\\' = LambdaCell
readCell 'L' = LambdaLiftCell False
readCell 'O' = LambdaLiftCell True
readCell '.' = EarthCell
readCell ' ' = EmptyCell
readCell _ = WallCell


encodeCell :: Cell -> Word8
encodeCell RobotCell = 1
encodeCell WallCell = 2
encodeCell (RockCell False) = 3
encodeCell (RockCell True) = 4
encodeCell LambdaCell = 5
encodeCell (LambdaLiftCell False) = 6
encodeCell (LambdaLiftCell True) = 7
encodeCell EarthCell = 8
encodeCell EmptyCell = 9


decodeCell :: Word8 -> Cell
decodeCell 1 = RobotCell
decodeCell 2 = WallCell
decodeCell 3 = RockCell False
decodeCell 4 = RockCell True
decodeCell 5 = LambdaCell
decodeCell 6 = LambdaLiftCell False
decodeCell 7 = LambdaLiftCell True
decodeCell 8 = EarthCell
decodeCell 9 = EmptyCell


oppositeDirection :: Direction -> Direction
oppositeDirection Left = Right
oppositeDirection Right = Left
oppositeDirection Up = Down
oppositeDirection Down = Up


worldSize :: World -> (Int, Int)
worldSize world =
  let (_, size) = bounds $ worldData world
  in size


worldInBounds :: World -> (Int, Int) -> Bool
worldInBounds world (columnIndex, rowIndex) =
  let (width, height) = worldSize world
  in (columnIndex >= 0)
     && (columnIndex < width)
     && (rowIndex >= 0)
     && (rowIndex < height)


worldCell :: World -> (Int, Int) -> Maybe Cell
worldCell world index =
  if worldInBounds world index
    then Just $ decodeCell $ worldData world ! index
    else Nothing


worldNearbyCell
  :: (Movement movement) => World -> (Int, Int) -> movement -> Maybe Cell
worldNearbyCell world index movement =
  worldCell world $ applyMovement movement index

robotSubmerged :: World -> Bool
robotSubmerged world =
    let (width, _) = worldSize world
    in any (\cell ->
                case cell of
                  Just RobotCell -> True
                  _ -> False) $
    concatMap (\rowIndex ->
               map (\columnIndex -> worldCell world (columnIndex, rowIndex))
                   [0 .. width - 1])
              [0 .. worldFloodingLevel world - 1]


robotDrowned :: World -> Bool
robotDrowned world = worldDrowningTicks world >= worldDrowningDuration world


visualize :: World -> [Text] -> IO ()
visualize world debugInformation = do
  let (width, height) = worldSize world
      water = worldFloodingLevel world
  putStr $ "\x1B[f\x1B[J"
  mapM_ (\rowIndex -> do
           let underwater = rowIndex < water
               background = if underwater then "44" else "40"
               earthBackground = if underwater then "46" else "43"
           mapM_ (\columnIndex -> do
                    putStr
                      $ case fromMaybe WallCell
                               $ worldCell world (columnIndex, rowIndex) of
                          RobotCell -> "\x1B[22;" ++ background ++ ";36m@"
                          WallCell -> "\x1B[22;47m "
                          RockCell _ -> "\x1B[22;" ++ background ++ ";37m●"
                          LambdaCell -> "\x1B[22;" ++ background ++ ";33mλ"
                          LambdaLiftCell False ->
                            "\x1B[22;" ++ background ++ ";37m◫"
                          LambdaLiftCell True ->
                            "\x1B[22;" ++ background ++ ";33m◫"
                          EarthCell -> "\x1B[22;" ++ earthBackground ++ "m "
                          EmptyCell -> "\x1B[22;" ++ background ++ "m "
                          -- _ -> "\x1B[22;1;41;30m?"
                    )
                 [0 .. width - 1]
           putStr "\n")
        [height - 1, height - 2 .. 0]
  putStr "\x1B[m"
  let informationStartColumn = width + 2
  mapM_ (\(lineIndex, line) -> do
            putStr $ "\x1B[" ++ (show $ lineIndex) ++ ";"
                     ++ (show informationStartColumn) ++ "f"
                     ++ (T.unpack line))
        (zip [2 ..]
             ((T.pack $ (show $ worldTicks world) ++ " ticks")
              : debugInformation))
  putStr $ "\x1B[" ++ (show $ height + 2) ++ ";1f"
  hFlush stdout


advanceWorld :: World -> Action -> StepResult
advanceWorld world action =
  let size@(width, height) = worldSize world
      --
      allIndices = [(columnIndex, rowIndex) |
                    columnIndex <- [0 .. width - 1],
                    rowIndex <- [0 .. height - 1]]
      --This is something worth testing
      (liftOpen, robotPos) =
          foldl' (\(noLambdas, robotPos) index ->
                      case fromMaybe EmptyCell $ worldCell world index of
                        LambdaCell -> (False, robotPos)
                        RobotCell -> (noLambdas, index)
                        _ -> (noLambdas, robotPos))
          (True, undefined)
          allIndices
      circumstances =
        Map.fromList
         $ mapMaybe (\index ->
                       let isEmpty path =
                             maybe False cellIsEmpty
                                   $ worldNearbyCell world index path
                       in fmap (\circumstance -> (index, circumstance))
                            $ case fromMaybe EmptyCell
                                     $ worldCell world index of
                                cell | cellFalls cell ->
                                  case () of
                                    () | isEmpty [Down] -> Just FallingDown
                                       | otherwise -> Nothing
                                       | otherwise -> Nothing)
                    allIndices
      newWorld =
          advanceWater $ world { worldData = makeWorldData size $
                                             map (advanceCell world) allIndices,
                                 worldTicks = 1 + worldTicks world
                               }
  in if robotDrowned newWorld
     then LossDrowned
     else Step newWorld


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
                              case soFar of
                                Nothing ->
                                    case worldNearbyCell world index path of
                                      Just cellAbove
                                          | cellFalls cellAbove ->
                                              Just $ cellAfterFalling cellAbove
                                      _ -> Nothing
                                _ -> soFar)
                    Nothing [[Up], [Up, Left], [Up, Right]]
           else cell)

advanceFallCell :: World -> Cell -> (Int, Int) -> Cell
advanceFallCell world cell index =
  let cellBelowEmpty = fromMaybe False $ fmap cellIsEmpty $ worldNearbyCell world index Down
  in if cellBelowEmpty
     then EmptyCell
     else cellAtRest cell


cellIsEmpty :: Cell -> Bool
cellIsEmpty EmptyCell = True
cellIsEmpty _ = False


cellFalls :: Cell -> Bool
cellFalls (RockCell _) = True
cellFalls _ = False


cellAtRest :: Cell -> Cell
cellAtRest (RockCell _) = RockCell False
cellAtRest cell = cell


cellAfterFalling :: Cell -> Cell
cellAfterFalling (RockCell _) = RockCell True
cellAfterFalling cell = cell


cellEnterable :: Cell -> Bool
cellEnterable LambdaCell = True
cellEnterable EarthCell = True
cellEnterable cell | cellIsEmpty cell = True
                   | otherwise = False


planner :: World -> Source (ResourceT IO) Action
planner world = do
  mapM_ (\_ -> yield $ MoveAction Up)
        [0 .. 100]
  return ()


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
          $ mapMaybe (\(index, encodedCell) ->
                        if decodeCell encodedCell == RobotCell
                          then Just (index, [])
                          else Nothing)
                     (assocs $ worldData world)

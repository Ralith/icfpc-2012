{-# LANGUAGE FlexibleInstances #-}
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
import qualified Data.Text as T
import Data.Word
import System.Environment
import System.Exit
import System.Posix.Unistd


data World =
  World {
      worldData :: UArray (Int, Int) Word8
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


main :: IO ()
main = do
  parameters <- getArgs
  case parameters of
    [worldFilePath] -> do
      world <- readWorld worldFilePath
      visualize world
      world <- runResourceT
                 $ planner world
                 $= slower
                 $$ C.foldM (\world action -> do
                             let world' = advanceWorld world action
                             lift $ visualize world'
                             return world')
                            world
      exitSuccess
    _ -> do
      putStrLn $ "Usage: bolder input.map"
      exitFailure


slower :: Conduit a (ResourceT IO) a
slower = do
  liftIO $ usleep 2000000
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
  let width = foldl' (\soFar line -> max soFar (T.length line)) 1 lines
      height = length lines
      associations =
       concat
       $ zipWith (\lineText rowIndex ->
                    zipWith (\cellCharacter columnIndex ->
                               ((columnIndex, rowIndex),
                                readCell cellCharacter))
                            (let lineChars = T.unpack lineText
                                 lineWidth = T.length lineText
                                 padding = take (width - lineWidth)
                                                (repeat ' ')
                             in lineChars ++ padding)
                            [0..])
                 lines
                 [0..]
  return $ makeWorld (width, height) associations


makeWorld :: (Int, Int) -> [((Int, Int), Cell)] -> World
makeWorld size associations =
  let worldData = array ((0, 0), size)
                      (map (\(index, cell) -> (index, encodeCell cell))
                           associations)
  in World {
         worldData = worldData
       }


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


visualize :: World -> IO ()
visualize world = do
  let (width, height) = worldSize world
  putStr $ "\x1B[f\x1B[J"
  mapM_ (\rowIndex -> do
           mapM_ (\columnIndex -> do
                    putStr
                      $ case fromMaybe WallCell
                               $ worldCell world (columnIndex, rowIndex) of
                          RobotCell -> "\x1B[22;40;36m@"
                          WallCell -> "\x1B[22;47m "
                          RockCell _ -> "\x1B[22;40;37m●"
                          LambdaCell -> "\x1B[22;40;33mλ"
                          LambdaLiftCell False -> "\x1B[22;40;37m◫"
                          LambdaLiftCell True -> "\x1B[22;40;33m◫"
                          EarthCell -> "\x1B[22;43m "
                          EmptyCell -> "\x1B[22;40m "
                          -- _ -> "\x1B[22;1;41;30m?"
                    )
                 [0 .. width - 1]
           putStr "\n")
        [0 .. height - 1]
  putStr "\x1B[m"


advanceWorld :: World -> Action -> World
advanceWorld world action =
  let size@(width, height) = worldSize world
      --
      allIndices = [(columnIndex, rowIndex) |
                    columnIndex <- [0 .. width - 1],
                    rowIndex <- [0 .. height - 1]]
      --This is something worth testing
      liftOpen = foldl' (\soFar index ->
                           case fromMaybe EmptyCell $ worldCell world index of
                             LambdaCell -> False
                             _ -> soFar)
                        True
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
  in makeWorld size
      $ map
          (\index ->
              (index, 
               let cell = fromMaybe EmptyCell $ worldCell world index
               in if cellFalls cell
                 then let cellBelowEmpty =
                            fromMaybe False
                             $ fmap cellIsEmpty
                             $ worldNearbyCell world index Down
                      in if cellBelowEmpty
                           then EmptyCell
                           else cellAtRest cell
                 else if cellIsEmpty cell
                        then fromMaybe cell
                               $ foldl' (\soFar path ->
                                           case soFar of
                                             Nothing ->
                                               case worldNearbyCell world index
                                                                    path of
                                                 Just cellAbove
                                                   | cellFalls cellAbove ->
                                                   Just
                                                    $ cellAfterFalling cellAbove
                                                 _ -> Nothing
                                             _ -> soFar)
                                       Nothing
                                       [[Up],
                                        [Up, Left],
                                        [Up, Right]]
                        else cell))
          allIndices


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


planner :: World -> Source (ResourceT IO) Action
planner world = do
  mapM_ (\_ -> yield $ MoveAction Up)
        [0 .. 10]
  return ()


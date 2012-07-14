module Visualization (visualize) where

import Data.Maybe
import qualified Data.Text as T
import System.IO

import World

visualize :: World -> [T.Text] -> IO ()
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
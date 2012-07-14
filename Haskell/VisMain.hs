{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Main (main) where

import Control.Monad.Trans
import Data.Conduit
import qualified Data.Conduit.Binary as C hiding (lines)
import qualified Data.Conduit.Text as C
import qualified Data.Conduit.List as C
import qualified Data.Text as T
import System.IO
import System.Environment
import System.Exit
import System.Posix.Unistd

import World
import Visualization
import Simulation
import Planning


readWorld :: FilePath -> IO World
readWorld filePath = readFile filePath >>= return . parseWorld . T.pack


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

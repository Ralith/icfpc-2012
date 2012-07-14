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
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Bolder


readWorld :: FilePath -> IO World
readWorld filePath = readFile filePath >>= return . parseWorld . T.pack


main :: IO ()
main = do
  debug <- fmap (isJust . lookup "DEBUG") getEnvironment
  parameters <- getArgs
  case parameters of
    [worldFilePath] -> do
      world <- readWorld worldFilePath
      visualize world []
      runResourceT $ planner world
                   $= slower
                   $$ C.foldM (\world action -> do
                                  when debug (lift $ void getLine)
                                  let result = advanceWorld' world action 
                                  case result of
                                    Step world' -> do
                                      lift $ visualize world' []
                                      return world'
                                    Win world' -> do
                                      lift $ visualize world' ["Win"]
                                      lift $ exitSuccess
                                    Abort world' -> do
                                      lift $ visualize world' ["Abort"]
                                      lift $ exitSuccess
                                    LossDrowned world' -> do
                                      lift $ visualize world' ["Lost: Drowned"]
                                      lift $ exitSuccess
                                    LossCrushed world' -> do
                                      lift $ visualize world' ["Lost: Crushed"]
                                      lift $ exitSuccess)
                              world
      exitSuccess
    _ -> do
      putStrLn $ "Usage: bolder input.map"
      exitFailure


slower :: Conduit a (ResourceT IO) a
slower = do
  liftIO $ usleep 50000
  maybeItem <- await
  case maybeItem of
    Just item -> do
      yield item
      slower
    Nothing -> return ()

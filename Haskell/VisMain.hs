{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Main (main) where

import Prelude hiding (Either(..))

import Control.Monad.Trans
import Data.Char
import Data.Conduit
import qualified Data.Conduit.Binary as C hiding (lines)
import qualified Data.Conduit.Text as C
import qualified Data.Conduit.List as C
import qualified Data.Text as T
import qualified Data.ByteString as B
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
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  debug <- fmap (lookup "DEBUG") getEnvironment
  parameters <- getArgs
  case parameters of
    [worldFilePath] -> do
      world <- readWorld worldFilePath
      visualize world []
      let actionSource = if maybe False (== "2") debug
                         then C.sourceHandle stdin $= extractAction
                         else planner world $= slower
      runResourceT $ actionSource
                   $$ C.foldM (\world action -> do
                                  when (maybe False (== "1") debug) (lift $ void getLine)
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

extractAction :: Monad m => Conduit B.ByteString m Action
extractAction = helper ""
    where
      helper accum = do
        maybeInput <- await
        case maybeInput of
          Just input ->
              let block = accum `B.append` input
              in case chr $ fromEnum $ B.head block of
                   'w' -> yield WaitAction >> helper (B.tail block)
                   'a' -> yield AbortAction >> helper (B.tail block)
                   'i' -> yield (MoveAction Up) >> helper (B.tail block)
                   'j' -> yield (MoveAction Left) >> helper (B.tail block)
                   'k' -> yield (MoveAction Down) >> helper (B.tail block)
                   'l' -> yield (MoveAction Right) >> helper (B.tail block)
                   -- '\x1B' ->
                   --     if B.length block >= 3
                   --     then let (escape, tail) = B.splitAt 3 input
                   --          in case escape of
                   --               "[A" -> yield $ MoveAction Up
                   --               "[B" -> yield $ MoveAction Down
                   --               "[C" -> yield $ MoveAction Right
                   --               "[D" -> yield $ MoveAction Left
                   --               _    -> helper tail
                   --     else helper block
                   _ -> helper (B.tail block)
          Nothing -> return ()

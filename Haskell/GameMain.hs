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
  parameters <- getArgs
  case parameters of
    [worldFilePath] -> do
      world <- readWorld worldFilePath
      visualize world Step []
      runResourceT $ C.sourceHandle stdin $= extractAction
                   $$ C.foldM (\(world, cmds) (cmd, action) -> do
                                  let before = worldRobotPosition world
                                  let (result, world') = advanceWorld' world action
                                  let after = worldRobotPosition world'
                                  lift $ visualize world' result
                                           [if safeMove world before after then "Safe" else "Deadly"]
                                  case result of
                                    Step -> return (world', cmd:cmds)
                                    _    -> lift $ exitSuccess)
                               (world, [])
      exitSuccess
    _ -> do
      putStrLn $ "Usage: bolder input.map"
      exitFailure


extractAction :: Monad m => Conduit B.ByteString m (Char, Action)
extractAction = helper ""
    where
      helper accum = do
        maybeInput <- await
        case maybeInput of
          Just input ->
              let block = accum `B.append` input
              in case chr $ fromEnum $ B.head block of
                   'w' -> yield ('w', WaitAction) >> helper (B.tail block)
                   'a' -> yield ('a', AbortAction) >> helper (B.tail block)
                   's' -> yield ('s', ShaveAction) >> helper (B.tail block)
                   'i' -> yield ('i', (MoveAction Up)) >> helper (B.tail block)
                   'j' -> yield ('j', (MoveAction Left)) >> helper (B.tail block)
                   'k' -> yield ('k', (MoveAction Down)) >> helper (B.tail block)
                   'l' -> yield ('l', (MoveAction Right)) >> helper (B.tail block)
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

{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Prelude hiding (Either(..))

import Data.Conduit
import Data.Text
import qualified Data.Conduit.Binary as C hiding (lines)
import qualified Data.Conduit.Text as C
import qualified Data.Conduit.List as C
import System.IO

import Bolder

encodeAction :: Action -> Char
encodeAction (MoveAction Left)  = 'L'
encodeAction (MoveAction Right) = 'R'
encodeAction (MoveAction Up)    = 'U'
encodeAction (MoveAction Down)  = 'D'
encodeAction WaitAction         = 'W'
encodeAction AbortAction        = 'A'


main :: IO ()
main = do
  world <- hGetContents stdin >>= return . parseWorld . pack
  planner world $= C.map (singleton . encodeAction) $= C.encode C.ascii $$ C.sinkHandle stdout
  return ()

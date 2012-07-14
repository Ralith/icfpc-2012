module Main where
    
import Criterion.Main
import Criterion
import Criterion.Config
import Control.Applicative 
import Data.Monoid

import Bolder
import qualified Data.Text as T

readWorld :: FilePath -> IO World
readWorld filePath = readFile filePath >>= return . parseWorld . T.pack

--This is just an example for testing
isLiftOpen' :: World -> Bool
isLiftOpen' x = isLiftOpen x (worldIndices x)

myConfig = defaultConfig { cfgReport = Last $ Just "profile.html" }

main = defaultMainWith myConfig (return ()) [
          bgroup "Simulation" [
            bench "isLiftOpen'" $ nfIO (isLiftOpen' <$> readWorld 
                                    "Maps/Toy/pile.map")
           ]
        ]
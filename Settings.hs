module Settings where

import qualified Data.Map as Map

import Expression

data InteractivityMode = Full | Steps

data Settings = Settings 
                { interactivityMode :: InteractivityMode
                , simplifyNumbers :: Bool
                , environment :: Map.Map String Expression
                }

defaultSettings = Settings { interactivityMode = Steps
                           , simplifyNumbers = False
                           , environment = Map.empty
                           }

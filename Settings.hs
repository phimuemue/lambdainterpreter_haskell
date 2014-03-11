module Settings where

data InteractivityMode = Full | Steps

data Settings = Settings 
                { interactivityMode :: InteractivityMode
                , simplifyNumbers :: Bool
                }

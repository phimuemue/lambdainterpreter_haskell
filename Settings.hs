{-# LANGUAGE DeriveDataTypeable #-} --needed for cmdargs
module Settings where

import qualified Data.Map as Map
import System.Console.CmdArgs

import Expression

data InteractivityMode = Full | Steps

data Arguments = Arguments { filename :: [String]
                           } deriving (Data, Typeable, Show)

defaultArguments = Arguments { filename = []
                   }

data Settings = Settings 
                { interactivityMode :: InteractivityMode
                , simplifyNumbers :: Bool
                , environment :: Map.Map String Expression
                , succName :: String
                , clargs :: Arguments
                }

defaultSettings = Settings { interactivityMode = Steps
                           , simplifyNumbers = False
                           , environment = Map.empty
                           , succName = "SUCC"
                           , clargs = defaultArguments
                           }

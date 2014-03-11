import Settings
import Expression (simplifyStep, alphaEquiv)
import Parser
import Command
import CommandParser
import Data.Maybe
import qualified Data.Map as Map
import System.IO
import System.Console.CmdArgs

stepPrint expr env = let nexpr = simplifyStep expr env in
                     if nexpr /= expr
                         then do putStrLn $ show nexpr
                                 stepPrint nexpr env
                         else putStrLn $ show nexpr

evalCommand cmd settings = case cmd of
    EmptyCmd -> repl settings
    SimpleExpression e -> do putStrLn $ show e
                             stepPrint e (environment settings)
                             repl settings
    LetStmt name e -> let curenv = environment settings in
                      let newenv = Map.insert name e curenv in
                      repl (settings {environment = newenv})
    LoadCmd f -> do abbrevs <- readLambdaFile f
                    let newenv = Map.union abbrevs $ environment settings
                    repl settings {environment = newenv}

repl settings = do putStr "> "
                   hFlush stdout
                   input <- getLine
                   if dropWhile (==' ') input == "" 
                       then repl settings 
                       else (case parseCommand (dropWhile (==' ') input) of
                             Nothing -> do putStrLn "Command not recognized"
                                           repl settings
                             Just cmd -> evalCommand cmd settings)

defaultEnvironment = Map.empty

getOnlyLetCommand cmd = case parseCommand (dropWhile (==' ') cmd) of
    Just (LetStmt name e) -> Just (name, e)
    _ -> Nothing

addEnvBindingFromLine env line = case getOnlyLetCommand line of
    Just (name, e) -> Map.insert name e env
    Nothing -> env

readLambdaFile f = do putStr "loading "
                      putStrLn f
                      content <- readFile f
                      let res = foldl addEnvBindingFromLine Map.empty (lines content)
                      return res

readLambdaFiles f = foldl doit (return Map.empty) f where
                    doit m p = do old <- m
                                  newabbrevs <- readLambdaFile p
                                  return $ Map.union old newabbrevs

main :: IO ()
main = do args <- cmdArgs $ defaultArguments
          putStrLn $ show args
          putStrLn "sdfklsjflksdjf"
          abbrevs <- readLambdaFiles $ filename args
          putStrLn $ "Abbrevs:"
          putStrLn $ show abbrevs
          let env = Map.union abbrevs $ environment defaultSettings
          repl $ defaultSettings {clargs = args, environment = env}

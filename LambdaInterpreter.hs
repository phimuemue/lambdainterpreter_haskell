import Settings
import Expression (simplifyStep, alphaEquiv)
import Parser
import Command
import CommandParser
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

main :: IO ()
main = do x <- cmdArgs $ defaultArguments
          putStrLn $ show x
          repl $ defaultSettings {clargs = x}

import Settings
import Expression (simplifyStep, alphaEquiv)
import Parser
import Command
import CommandParser
import qualified Data.Map as Map
import System.IO


stepPrint expr env = let nexpr = simplifyStep expr env in
                     if nexpr /= expr
                         then do putStrLn $ show nexpr
                                 stepPrint nexpr env
                         else putStrLn $ show nexpr

evalCommand cmd settings = case cmd of
    SimpleExpression e -> do putStrLn $ show e
                             stepPrint e (environment settings)
                             repl settings
    LetStmt name e -> let curenv = environment settings in
                      let newenv = Map.insert name e curenv in
                      repl (settings {environment = newenv})

repl settings = do putStr "> "
                   hFlush stdout
                   input <- getLine
                   (case parseCommand input of
                    Nothing -> do putStrLn "Command not recognized"
                                  repl settings
                    Just cmd -> evalCommand cmd settings)

defaultEnvironment = Map.empty

main :: IO ()
main = repl defaultSettings

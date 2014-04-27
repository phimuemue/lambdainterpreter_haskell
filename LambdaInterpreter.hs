import Settings
import Expression
import Parser
import Command
import CommandParser
import PrettyPrinter
import Data.Maybe
import qualified Data.Map as Map
import System.IO
import System.Console.CmdArgs
import System.Posix.Signals
import Control.Concurrent
import Control.Concurrent.MVar

-- tryFunctions tries to apply one function after another to an expression
-- until some change occurs. If no function yields a change, it terminates.
-- Note: the settings have to be capsuled in the functions given in the
-- list.
tryFunctions :: [Expression -> Expression] -> Expression -> IO Expression
tryFunctions [] term = return term
tryFunctions (f:fs) expr =
    let taggedExpr = f expr in
    let newExpr = applyTags taggedExpr in
    if taggedExpr /= expr
    then do prettyPrint taggedExpr
            tryFunctions (f:fs) newExpr
    else tryFunctions fs expr

stepPrint :: Expression -> Settings -> IO Expression
stepPrint expr settings = 
    tryFunctions
    [ allAbbreviationTags $ environment settings
    , normalOrderTags
    ]
    expr

tryFunctionsInteractive :: [Expression -> Expression] -> Expression -> IO Expression
tryFunctionsInteractive [] term = return term
tryFunctionsInteractive (f:fs) expr =
    let taggedExpr = f expr in
    let newExpr = applyTags taggedExpr in
    if taggedExpr /= expr
    then do prettyPrint taggedExpr
            c <- howToContinue
            (case c of
                'a' -> return newExpr
                'c' -> tryFunctions (f:fs) newExpr
                _   -> tryFunctionsInteractive (f:fs) newExpr)
    else tryFunctionsInteractive fs expr
    where howToContinue = getSingleKeyPress 
                          "How to continue? (A: abort, C: complete, _: step)"

interactivePrint :: Expression -> Settings -> IO Expression
interactivePrint expr settings =
    tryFunctionsInteractive
    [ allAbbreviationTags $ environment settings
    , normalOrderTags
    ]
    expr

normalizePrint :: Expression -> Settings -> IO Expression
normalizePrint expr settings = let env = environment settings in
                               let nexpr = simplifyComplete expr env in
                               return nexpr

consumeExpression strategy = case strategy of
    Full -> normalizePrint
    Steps -> stepPrint
    Interactive -> interactivePrint

evalCommand cmd settings = case cmd of
    EmptyCmd -> repl settings
    SimpleExpression e -> do prettyPrint e
                             result <- computeMe e settings
                             putStrLn $ show result
                             repl settings
                          where computeMe = consumeExpression
                                            (interactivityMode settings)
    LetStmt name e -> let curenv = environment settings in
                      let newenv = Map.insert name e curenv in
                      repl (settings {environment = newenv})
    LoadCmd f -> do abbrevs <- readLambdaFile f
                    let newenv = Map.union abbrevs $ environment settings
                    repl settings {environment = newenv}
    SetCmd f -> case f of
                "fulleval" -> repl settings {interactivityMode = Full}
                "stepeval" -> repl settings {interactivityMode = Steps}
                "inteval" -> repl settings {interactivityMode = Interactive}
                _ -> do 
                    _ <- putStrLn $ "Unknown option: " ++ f
                    repl settings

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
                      let res = foldl addEnvBindingFromLine Map.empty $
                                lines content
                      return res

readLambdaFiles f = foldl doit (return Map.empty) f where
                    doit m p = do old <- m
                                  newabbrevs <- readLambdaFile p
                                  return $ Map.union old newabbrevs

interruptionHandler itr = do i <- takeMVar itr
                             putStrLn "Interrupted"
                             putMVar itr (not i)

getSingleKeyPress :: String -> IO Char
getSingleKeyPress msg = do
  hSetBuffering stdin NoBuffering
  putStr msg
  hFlush stdout
  x <- getChar
  hSetBuffering stdin LineBuffering
  putStrLn ""
  return x

main :: IO ()
main = do args <- cmdArgs $ defaultArguments
          itr <- newMVar False -- to capture Ctrl-C signal
          --installHandler sigINT (Catch $ interruptionHandler itr) Nothing
          putStrLn $ show args
          putStrLn "sdfklsjflksdjf"
          abbrevs <- readLambdaFiles $ filename args
          putStrLn $ "Abbrevs:"
          putStrLn $ show abbrevs
          let env = Map.union abbrevs $ environment defaultSettings
          repl $ defaultSettings { clargs = args
                                 , environment = env
                                 , interruption = itr
                                 }

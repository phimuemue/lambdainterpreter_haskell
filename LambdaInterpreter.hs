import Settings
import Expression
import Command
import CommandParser
import Taggers
import PrettyPrinter
import InteractiveTagger

import qualified Data.Map as Map
import System.IO
import System.Console.CmdArgs
import Control.Concurrent
import Data.Maybe
import Data.Either.Combinators

tryFunctions :: [Expression -> TaggedExpression] -> Expression -> TaggedExpression
tryFunctions [] term = Left term
tryFunctions (f:fs) expr =
    let taggedExpr = f expr in
    if isRight taggedExpr then taggedExpr else tryFunctions fs expr

evaluateExpression :: 
    (Expression -> IO TaggedExpression) -> Expression -> IO TaggedExpression
evaluateExpression tagfn expr =
    do taggedExpr <- tagfn expr
       let newexpr = applyTags $ fromTaggedExpression expr taggedExpr
       -- TODO: The following must be doable more elegant
       -- In particular the conversion fromRight' and afterwards back
       -- to Right must be solved in another manner
       if isRight taggedExpr
       then evaluateExpression tagfn newexpr
       else return $ Left expr

tagAndPrint :: (Expression -> TaggedExpression) -> Expression -> IO TaggedExpression
tagAndPrint tagfn expr =
    let taggedExpr = tagfn expr in
    if isRight taggedExpr then do prettyPrint $ fromRight' taggedExpr
                                  return taggedExpr
                         else return taggedExpr

stepPrint :: Expression -> Settings -> IO TaggedExpression
stepPrint expr settings = evaluateExpression (tagAndPrint tagfn) expr
    where tagfn = tryFunctions 
                  [ allAbbreviationTags settings
                  , normalOrderTags
                  ]

interactivePrint :: Expression -> Settings -> IO TaggedExpression
interactivePrint expr settings = 
    evaluateExpression (interactiveTags settings) expr

normalizePrint :: Expression -> Settings -> IO TaggedExpression
normalizePrint expr settings = evaluateExpression (return . tagfn) expr
    where tagfn = tryFunctions
                  [ allAbbreviationTags settings
                  , normalOrderTags
                  ]

consumeExpression :: 
    InteractivityMode -> Expression -> Settings -> IO TaggedExpression
consumeExpression strategy = case strategy of
    Full -> normalizePrint
    Steps -> stepPrint
    Interactive -> interactivePrint

evalCommand :: Command -> Settings -> IO ()
evalCommand cmd settings = case cmd of
    EmptyCmd -> repl settings
    SimpleExpression e -> do prettyPrint e
                             result <- computeMe e settings
                             putStrLn $ show (simpleFromTaggedExpression result)
                             repl settings 
                                  {environment = 
                                   Map.insert "_" (simpleFromTaggedExpression result) (environment settings)}
                          where computeMe = consumeExpression
                                            (interactivityMode settings)
                                simpleFromTaggedExpression (Left e) = e
                                simpleFromTaggedExpression (Right e) = e
    LetStmt n e -> let curenv = environment settings in
                      let newenv = Map.insert n e curenv in
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

repl :: Settings -> IO ()
repl settings = do putStr "> "
                   hFlush stdout
                   input <- getLine
                   if dropWhile (==' ') input == "" 
                       then repl settings 
                       else (case parseCommand (dropWhile (==' ') input) of
                             Nothing -> do putStrLn "Command not recognized"
                                           repl settings
                             Just cmd -> evalCommand cmd settings)

getOnlyLetCommand :: String -> Maybe (String, Expression)
getOnlyLetCommand cmd = case parseCommand (dropWhile (==' ') cmd) of
    Just (LetStmt n e) -> Just (n, e)
    _ -> Nothing

addEnvBindingFromLine :: Environment -> String -> Environment
addEnvBindingFromLine env line = case getOnlyLetCommand line of
    Just (n, e) -> Map.insert n e env
    Nothing -> env

readLambdaFile :: String -> IO Environment
readLambdaFile f = do putStr "loading "
                      putStrLn f
                      content <- readFile f
                      let res = foldl addEnvBindingFromLine Map.empty $
                                lines content
                      return res

readLambdaFiles :: [String] -> IO Environment
readLambdaFiles f = foldl doit (return Map.empty) f where
                    doit m p = do old <- m
                                  newabbrevs <- readLambdaFile p
                                  return $ Map.union old newabbrevs

interruptionHandler :: MVar Bool -> IO ()
interruptionHandler itr = do i <- takeMVar itr
                             putStrLn "Interrupted"
                             putMVar itr (not i)

main :: IO ()
main = do myargs <- cmdArgs $ defaultArguments
          itr <- newMVar False -- to capture Ctrl-C signal
          --installHandler sigINT (Catch $ interruptionHandler itr) Nothing
          putStrLn $ show myargs
          abbrevs <- readLambdaFiles $ filename myargs
          putStrLn $ "Abbrevs:"
          putStrLn $ show abbrevs
          let env = Map.union abbrevs $ environment defaultSettings
          repl $ defaultSettings { clargs = myargs
                                 , environment = env
                                 , interruption = itr
                                 }

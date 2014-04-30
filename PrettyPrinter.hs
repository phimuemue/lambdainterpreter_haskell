module PrettyPrinter where

import Expression

import Data.Maybe
import System.Console.ANSI

setSGRAbbrev :: IO ()
setSGRAbbrev = setSGR [SetColor Foreground Vivid Green]
setSGRFunCall :: IO ()
setSGRFunCall = setSGR [SetColor Foreground Vivid Red]
setSGRFunArg :: IO ()
setSGRFunArg = setSGR [SetColor Foreground Vivid Blue]
setSGREta :: IO ()
setSGREta = setSGR [SetColor Foreground Vivid Green]

prettyPrint :: Expression -> IO ()
prettyPrint term = do aux False term
                      setSGR []
                      putStrLn ""
    where aux plbd t = case t of
            Variable abb s -> do (if isJust abb then setSGRAbbrev else return ())
                                 putStr s
                                 setSGR []
            Abstraction eta x f -> do (if eta then setSGREta else return ())
                                      case (x, f) of
                                           (_, Abstraction _ _ _) -> do putStr (if plbd then " " else "\\")
                                                                        putStr x 
                                                                        aux True f
                                           (_, _) -> do putStr (if plbd then " " else "\\")
                                                        putStr x
                                                        putStr ". "
                                                        aux True f
                                      setSGR []

            Application beta f x -> if beta
                                    then do (case (f, x) of
                                             (Abstraction _ _ _, Application _ _ _) -> do putStr "(" 
                                                                                          setSGRFunCall
                                                                                          putStr $ show f 
                                                                                          setSGR []
                                                                                          putStr ") (" 
                                                                                          setSGRFunArg
                                                                                          putStr $ show x 
                                                                                          setSGR []
                                                                                          putStr ")"
                                             (Abstraction _ _ _, Abstraction _ _ _) -> do putStr "(" 
                                                                                          setSGRFunCall
                                                                                          putStr $ show f 
                                                                                          setSGR []
                                                                                          putStr ") (" 
                                                                                          setSGRFunArg
                                                                                          putStr $ show x 
                                                                                          setSGR []
                                                                                          putStr ")"
                                             (Abstraction _ _ _, _) -> do putStr "(" 
                                                                          setSGRFunCall
                                                                          putStr $ show f 
                                                                          setSGR []
                                                                          putStr ") "
                                                                          setSGRFunArg
                                                                          putStr $ show x
                                                                          setSGR []
                                             (_, Variable _ _) -> do putStr $ show f
                                                                     putStr " "
                                                                     putStr $ show x
                                             (_, _) -> do putStr $ show f 
                                                          putStr " ("
                                                          putStr $ show x
                                                          putStr ")")

                                            setSGR []
                                    else case (f, x) of
                                            (Abstraction _ _ _, Application _ _ _) -> do putStr "(" 
                                                                                         aux False f 
                                                                                         putStr ") (" 
                                                                                         aux False x 
                                                                                         putStr ")"
                                            (Abstraction _ _ _, Abstraction _ _ _) -> do putStr "(" 
                                                                                         aux False f 
                                                                                         putStr ") (" 
                                                                                         aux False x 
                                                                                         putStr ")"
                                            (Abstraction _ _ _, _) -> do putStr "(" 
                                                                         aux False f 
                                                                         putStr ") "
                                                                         aux False x
                                            (_, Variable _ _) -> do aux False f
                                                                    putStr " "
                                                                    aux False x
                                            (_, _) -> do aux False f 
                                                         putStr " ("
                                                         aux False x
                                                         putStr ")"

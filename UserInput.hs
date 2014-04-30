module UserInput where

import System.IO

getSingleKeyPress :: String -> IO Char
getSingleKeyPress msg = do
  hSetBuffering stdin NoBuffering
  putStr msg
  hFlush stdout
  hSetEcho stdin False
  x <- getChar
  hSetBuffering stdin LineBuffering
  putStrLn ""
  hSetEcho stdin True
  return x

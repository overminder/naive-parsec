import System.Environment

import Regex

main = do
  args <- getArgs
  case args of
    [pattern, str] -> case compile pattern of
      Nothing -> putStrLn "Invalid regex"
      Just rawRegex -> do
        putStrLn $ "[Debug] Compiled regex: " ++ show rawRegex
        case match rawRegex str of
          Just out -> print out
          Nothing -> putStrLn "No match"
    _ -> putStrLn "Usage: ./Main [pattern] [string-to-match]"


module RegexGroup (
  RegexParser,
  runRegexParser
) where

import Parser
import Control.Monad.Reader

import Regex ( Regex(..)
             , Range(..)
             )

-- Stores the current group
type RegexParser = ReaderT Int (Parser Char)
type RegexMatcher = Parser Char (Int, String)

runRegexParser :: RegexParser a -> String -> Maybe a
runRegexParser p pattern = parse (runReaderT p 1) pattern


module Parser where

import qualified Data.ByteString as BS

newtype Parser e s a = Parser
  { runParser :: s -> Either e (a, s) }

type FilePackParser = Parser String BS.ByteString

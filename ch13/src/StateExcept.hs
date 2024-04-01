{-# LANGUAGE OverloadedStrings #-}
module StateExcept where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text
import ExceptT
import StateT

type ParseError = Text
type ParseState = Text

type Parser = StateT ParseState (Except ParseError)

runParser :: Text -> Parser a -> Either ParseError a
runParser input parser =
  runExcept $ evalStateT parser input

parseChar :: Parser Char
parseChar = do
  parseState <- get
  case Text.uncons parseState of
    Nothing -> liftStateT $ throwError "end of input"
    Just (c, rest) -> do
      put rest
      pure c

char :: Char -> Parser ()
char expectedChar = do
  actualChar <- parseChar
  when (expectedChar /= actualChar) $
    liftStateT $ throwError "Invalid character"

{-# LANGUAGE OverloadedStrings #-}
module ExceptState where

import Data.Text
import qualified Data.Text as Text
import Control.Monad (when)

import ExceptT
import StateT

type ParseError = Text
type ParseState = Text

type Parser = ExceptT ParseError (State ParseState)

runParser :: Text -> Parser a -> Either ParseError a
runParser input parser =
  evalState (runExceptT parser) input

parseChar :: Parser Char
parseChar = do
  parseState <- succeed get
  case Text.uncons parseState of
    Nothing -> throwError "end of input"
    Just (c, rest) -> do
      succeed $ put rest
      pure c

char :: Char -> Parser ()
char expectedChar = do
  actualChar <- parseChar
  when (expectedChar /= actualChar) $
    throwError "Invalid character"


{-# LANGUAGE OverloadedStrings #-}
module Apps.Mtl.StateMonad1 where

import Control.Monad (when)
import Libs.Mtl.Control.Monad.State
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text

type Parser a = State Text (Either Text a)

data FullName = FullName
  { first :: Text
  , middle :: Text
  , last :: Text
  } deriving (Show, Eq)

takeUntil :: (Char -> Bool) -> Parser Text
takeUntil predicate = do
  oldState <- get
  let (nextVal, rest) = Text.break predicate oldState
  put rest
  pure (pure nextVal)

dropChar :: Parser ()
dropChar = do
  parseState <- get
  -- Avoid error with last word
  if Text.null parseState
  then pure (Right ())
  else do
    let newState = Text.tail parseState
    newState `seq` put newState
    pure (Right ())

word :: Parser Text
word = do
  nextWord <- takeUntil isSpace
  case nextWord of
    Right "" -> pure (Left "missing word")
    _ -> do
      _ <- dropChar
      pure nextWord

parseFullName :: Parser FullName
parseFullName = do -- Parser monad
  firstName <- word
  middleName <- word
  lastName <- word
  pure $ do -- Either monad
    firstName' <- firstName
    middleName' <- middleName
    lastName' <- lastName
    pure $ FullName firstName' middleName' lastName'

{-
Original implementation:

dropChar :: Parser ()
dropChar = do
  parseState <- get
  let newState = Text.tail parseState
  put newState
  pure (Right ())

The reason `dropChar` doesn't raises an error on `Text.tail` after `takeUntil`
reads the last word, is because in `dropChar`, `put newState` will save in
state a "thunk" with newState, and since there are no more words to be
processed, that "thunk" won't be evaluated and it won't raise an exception.

That said, if you force the "thunk" to be evaluated when calling `put`, e.g:

newState `seq` put newState

then the program will raise an exception when reading the last word.

uncaught exception: ErrorCall
Data.Text.tail: empty input
-}

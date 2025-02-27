module Apps.Mtl.StateDemo1 where

import Std.Control.Monad.State
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
  let newState = Text.tail parseState
  put newState
  pure (Right ())

word :: Parser Text
word = do
  nextWord <- takeUntil isSpace
  _ <- dropChar
  pure nextWord

parseFullName :: Parser FullName
parseFullName = do
  firstName <- word
  middleName <- word
  lastName <- word
  pure $ do
    firstName' <- firstName
    middleName' <- middleName
    lastName' <- lastName
    pure $ FullName firstName' middleName' lastName'


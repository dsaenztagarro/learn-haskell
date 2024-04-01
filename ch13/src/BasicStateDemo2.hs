module BasicStateDemo2 where
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import State

-- ghci> execState parseFullName "David Saenz Tagarro "
-- ""
-- ghci> evalState parseFullName "David Saenz Tagarro "
-- Right (FullName {first = "David", middle = "Saenz", last = "Tagarro"})

type Parser a = State Text (Either Text a)

data FullName = FullName
  { first :: Text
  , middle :: Text
  , last :: Text
  } deriving Show

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


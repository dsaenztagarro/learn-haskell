module FailingStatefulParser where

import Control.Monad
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import State
import qualified State

-- ghci> :set -XOverloadedStrings
-- ghci> evalParser parseFullName "David Saenz Tagarro"
-- Right (FullName {first = "David", middle = "Saenz", last = "Tagarro"})

data FullName = FullName
  { first :: Text
  , middle :: Text
  , last :: Text
  } deriving Show

newtype Parser a = Parser
  {runParser :: State Text (Either String a)}

evalParser :: Parser a -> Text -> Either String a
evalParser = State.evalState . runParser

parse :: Parser a -> Text -> (Either String a, Text)
parse = State.runState . runParser

instance Functor Parser where
  -- ghci> :t (fmap . fmap)
  -- (fmap . fmap)
  --   :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
  --
  --   Functor f1 is State
  --   Functor f2 is Either
  --
  fmap f parser =
    Parser $ (fmap . fmap) f (runParser parser)

instance Applicative Parser where
  -- ghci> :t (pure . pure)
  -- (pure . pure) :: (Applicative f1, Applicative f2) => a -> f1 (f2 a)
  --
  --   Functor f1 is State
  --   Functor f2 is Either
  --
  pure a = Parser $ (pure . pure) a
  f <*> a = Parser $ do
    f' <- runParser f
    a' <- runParser a
    pure $ f' <*> a'

instance Monad Parser where
  return = pure
  a >>= f = Parser $ do
    val <- runParser a
    case val of
      Left err -> pure (Left err)
      Right val' -> runParser (f val')

parseError :: String -> Parser a
parseError errMsg = Parser $ pure (Left errMsg)

parseGet :: Parser Text
parseGet = Parser (Right <$> State.get)

parsePut :: Text -> Parser ()
parsePut newState = Parser $ Right <$> State.put newState

takeUntil :: (Char -> Bool) -> Parser Text
takeUntil predicate = do
  oldState <- parseGet
  let (nextVal, rest) = Text.break predicate oldState
  parsePut rest
  pure nextVal

optionally :: Parser () -> Parser ()
optionally originalParser = Parser $ do
  oldState <- State.get
  result <- runParser originalParser
  case result of
    Left _err -> State.put oldState
    _success -> pure ()
  pure $ Right ()

word :: Parser Text
word = do
  nextWord <- takeUntil isSpace
  when (Text.null nextWord) $
    parseError "unexpected end of input"
  optionally dropChar
  pure nextWord

dropChar :: Parser ()
dropChar = do
  parseState <- parseGet
  case Text.uncons parseState of
    Nothing -> parseError "unexpected end of input"
    Just (_, rest) -> parsePut rest

parseFullName :: Parser FullName
parseFullName = FullName <$> word <*> word <*> word

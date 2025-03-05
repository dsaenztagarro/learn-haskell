{-# LANGUAGE OverloadedStrings #-}
module Libs.Mtl.Control.Monad.StateSpec where

import Test.Hspec
import Libs.Mtl.Control.Monad.State (State)
import qualified Libs.Mtl.Control.Monad.State as State

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Char (isSpace)

------------------
-- State Action
------------------

stateAction :: State Int String
stateAction  =
  appendLineWithIndent "hello" ""
  >>= appendLineWithIndent "world"
  >>= appendLineWithIndent "love,"
  >>= appendLineWithIndent "George"

appendLineWithIndent :: String -> String -> State Int String
appendLineWithIndent message previousMessage = do
  indentLevel <- State.get
  let
    nextIndentLevel = indentLevel + 2
    indent = replicate nextIndentLevel '*'
    output = previousMessage <> indent <> message <> "\n"
  State.put nextIndentLevel
  pure output

------------------------------------
-- State Parser with nested monad
------------------------------------

type Parser a = State Text (Either Text a)

data FullName = FullName
  { first :: Text
  , middle :: Text
  , last :: Text
  } deriving (Show, Eq)

takeUntil :: (Char -> Bool) -> Parser Text
takeUntil predicate = do
  oldState <- State.get
  let (nextVal, rest) = Text.break predicate oldState
  State.put rest
  pure (pure nextVal)

dropChar' :: Parser ()
dropChar' = do
  parseState <- State.get
  let newState = Text.tail parseState
  State.put newState
  pure (Right ())

word :: Parser Text
word = do
  nextWord <- takeUntil isSpace
  _ <- dropChar'
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

----------------------------------------------------------
-- State Parser with nested monad with common interface
----------------------------------------------------------

newtype Parser' a = Parser'
  {runParser' :: State Text (Either String a)}

evalParser :: Parser' a -> Text -> Either String a
evalParser = State.evalState . runParser'

parse :: Parser' a -> Text -> (Either String a, Text)
parse = State.runState . runParser'

instance Functor Parser' where
  fmap f parser =
    Parser' $ (fmap . fmap) f (runParser' parser)
{-
ghci> :t (fmap . fmap)
(fmap . fmap)
  :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)

  Functor f1 is State
  Functor f2 is Either
-}

instance Applicative Parser' where
  pure a = Parser' $ (pure . pure) a
  f <*> a = Parser' $ do
    f' <- runParser' f
    a' <- runParser' a
    pure $ f' <*> a'
{-
ghci> :t (pure . pure)
(pure . pure) :: (Applicative f1, Applicative f2) => a -> f1 (f2 a)

  Functor f1 is State
  Functor f2 is Either
-}

instance Monad Parser' where
  return = pure
  a >>= f = Parser' $ do
    val <- runParser' a
    case val of
      Left err -> pure (Left err)
      Right val' -> runParser' (f val')

parseError :: String -> Parser' a
parseError errMsg = Parser' $ pure (Left errMsg)

parseGet :: Parser' Text
parseGet = Parser' (Right <$> State.get)

parsePut :: Text -> Parser' ()
parsePut newState = Parser' $ Right <$> State.put newState

takeUntil' :: (Char -> Bool) -> Parser' Text
takeUntil' predicate = do
  oldState <- parseGet
  let (nextVal, rest) = Text.break predicate oldState
  parsePut rest
  pure nextVal

optionally :: Parser' () -> Parser' ()
optionally originalParser' = Parser' $ do
  oldState <- State.get
  result <- runParser' originalParser'
  case result of
    Left _err -> State.put oldState
    _success -> pure ()
  pure $ Right ()

word' :: Parser' Text
word' = do
  nextWord <- takeUntil' isSpace
  when (Text.null nextWord) $
    parseError "unexpected end of input"
  optionally dropChar
  pure nextWord

dropChar :: Parser' ()
dropChar = do
  parseState <- parseGet
  case Text.uncons parseState of
    Nothing -> parseError "unexpected end of input"
    Just (_, rest) -> parsePut rest

parseFullName' :: Parser' FullName
parseFullName' = FullName <$> word' <*> word' <*> word'

spec :: Spec
spec = do
  describe "stateAction" $ do
    describe "evalState" $ do
      it "keeps level of indentation in the mutable state" $ do
        State.evalState stateAction 0 `shouldBe` "**hello\n****world\n******love,\n********George\n"

    describe "execState" $ do
      it "keeps level of indentation in the mutable state" $ do
        State.execState stateAction 0 `shouldBe` 8

  describe "stateParser with nested monad" $ do
    describe "evalState" $ do
      it "returns full name" $ do
        State.evalState parseFullName "Foo Bar Baz" `shouldBe` (Right $ FullName "Foo" "Bar" "Baz")

  describe "stateParser with nested monad with common interface" $ do
    describe "evalState" $ do
      it "returns full name" $ do
        evalParser parseFullName' "Foo Bar Baz" `shouldBe` (Right $ FullName "Foo" "Bar" "Baz")

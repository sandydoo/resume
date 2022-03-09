{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Arrow (left)
import Data.Aeson as Json
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO as TIO
import Data.Time (Day)
import Options.Applicative
import Text.DocLayout (render)
import qualified Text.DocTemplates as Template

data Flags = Flags
  { flagPathToJson :: FilePath,
    flagPathToTemplate :: FilePath
  }

flagParser :: Parser Flags
flagParser =
  Flags
    <$> (argument str (metavar "me.json"))
    <*> (argument str (metavar "template.md"))

data Problem
  = CannotDecodeJson String
  | CannotCompileTemplate Text
  deriving stock (Eq, Show)

data Basics = Basics
  { name :: Text,
    dateOfBirth :: Day,
    email :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

main :: IO ()
main = do
  flags <- execParser opts
  process flags >>= \case
    Left problem -> reportProblem problem
    Right text -> TIO.putStrLn text
  where
    opts =
      info
        flagParser
        ( fullDesc
            <> progDesc "Convert a JSON file about you into some human-readable format"
            <> header "What does this do?"
        )

process :: Flags -> IO (Either Problem Text)
process (Flags pathToJson pathToTemplate) = runExceptT $ do
  jsonRawData <- liftIO $ BL.readFile pathToJson
  jsonData <- ExceptT $ pure $ decodeJsonResume jsonRawData
  template <- ExceptT $ left (CannotCompileTemplate . toText) <$> Template.compileTemplateFile pathToTemplate
  pure $ render Nothing $ Template.renderTemplate template (object ["basics" .= toJSON jsonData])

decodeJsonResume :: BL.ByteString -> Either Problem Basics
decodeJsonResume = left CannotDecodeJson . Json.eitherDecode

reportProblem :: Problem -> IO ()
reportProblem problem = TIO.putStrLn $
  case problem of
    CannotDecodeJson errorMsg -> show errorMsg
    CannotCompileTemplate templateErrors -> templateErrors

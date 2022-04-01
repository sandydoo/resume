{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}


module Main (main) where

import           Control.Arrow              (left)
import           Control.Exception          hiding (throwIO)
import           Control.Lens               hiding ((<.>))
import qualified Data.Aeson                 as Json
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text                  as Text
import           Data.Time.Clock
import           Data.Time.Format
import qualified Options.Applicative        as CLI
import qualified System.Directory           as System
import qualified System.Exit                as System
import           System.FilePath            ((<.>), (</>))
import qualified System.FilePath            as FP
import qualified System.Process             as Proc
import qualified Text.DocLayout             as Template
import qualified Text.DocTemplates          as Template
import           UnliftIO.Exception

data Flags = Flags
  { flagPathToJson     :: FilePath
  , flagPathToTemplate :: FilePath
  }

flagParser :: CLI.Parser Flags
flagParser =
  Flags
    <$> CLI.argument CLI.str (CLI.metavar "me.json")
    <*> CLI.strOption
      ( CLI.long "template"
        <> CLI.metavar "template.tex"
        <> CLI.help "The template to fill"
      )

data TemplateFormat
  = LaTex
  deriving stock (Eq, Show)

data Problem
  = CannotDecodeJson String
  | CannotCompileTemplate String
  | CannotRunBuild Text
  | CannotGetCommitHash String
  | UnsupportedTemplateFormat String
  deriving stock (Eq, Show)

instance Exception Problem where
  displayException = reportProblem

main :: IO ()
main = flip catchAny handler $ do
  flags <- CLI.execParser $
    CLI.info flagParser
      ( CLI.fullDesc
          <> CLI.progDesc "Convert a JSON file about you into some human-readable format"
      )

  let fileName = FP.takeFileName $ flagPathToTemplate flags
  let baseName = FP.takeBaseName $ flagPathToTemplate flags
  let format = flip fromMaybe (detectFormat fileName) $
                 throw $ UnsupportedTemplateFormat (FP.takeExtension fileName)

  let tmpDir = "temp"
  System.createDirectoryIfMissing False tmpDir
  BL.writeFile (tmpDir </> fileName) . encodeUtf8 =<< fillTemplate format flags

  exitCode <-
    case format of
      LaTex -> do
        runCommandWithDir (Just tmpDir) "latexmk" ["-xelatex", show fileName]

  case exitCode of
    (System.ExitSuccess, _, _) -> do
      BL8.putStrLn "✍️  Resume created"
      let outputPdf = baseName <.> "pdf"
      System.copyFile (tmpDir </> outputPdf) outputPdf

    (System.ExitFailure _, _, err) -> do
      log <- readFileAsText (tmpDir </> baseName <.> "log")
      throw $ CannotRunBuild (Text.append (toText err) log)


  where
    handler e = BL8.hPutStrLn stderr (encodeUtf8 . Text.pack $ displayException e) >> exitFailure

fillTemplate :: TemplateFormat -> Flags -> IO Text
fillTemplate format Flags {..} = do
    hash <- getCommitShortHash
    date <- getCurrentFormattedDate
    jsonData <- throwLeft $ decodeJsonResume <$> readFileAsByteString flagPathToJson
    compiledTemplate <- throwLeft $ compileTemplate =<< readFileAsText flagPathToTemplate

    pure $ Template.render Nothing $
      Template.renderTemplate compiledTemplate $
        escapeJson format jsonData & _Object . at "version" ?~ Json.String (hash <> date)

  where
    throwLeft = fmap (either throw id)

readFileAsByteString :: MonadIO m => FilePath -> m BL.ByteString
readFileAsByteString = liftIO . BL.readFile

readFileAsText :: MonadIO m => FilePath -> m Text
readFileAsText path = decodeUtf8 @Text @BL.ByteString <$> liftIO (BL.readFile path)

escapeJson :: TemplateFormat -> Json.Value -> Json.Value
escapeJson LaTex = escapeLatex

escapeLatex :: Json.Value -> Json.Value
escapeLatex (Json.Object object) = Json.Object $ fmap escapeLatex object
escapeLatex (Json.Array array) = Json.Array $ fmap escapeLatex array
escapeLatex (Json.String text) = Json.String $ Text.concatMap escape text
  where
    escape :: Char -> Text
    escape '_'  = "\\_"
    escape '&'  = "\\&"
    escape '#'  = "\\#"
    escape char = Text.singleton char

escapeLatex other = other

decodeJsonResume :: BL.ByteString -> Either Problem Json.Value
decodeJsonResume = left CannotDecodeJson . Json.eitherDecode

compileTemplate :: Text -> IO (Either Problem (Template.Template Text))
compileTemplate template = left CannotCompileTemplate <$> Template.compileTemplate "" template

detectFormat :: FilePath -> Maybe TemplateFormat
detectFormat path =
  case FP.takeExtension path of
    ".tex" -> Just LaTex
    _      -> Nothing

reportProblem :: Problem -> String
reportProblem problem =
  case problem of
    CannotDecodeJson e -> "There’s something wrong with the JSON file.\n" <> e
    CannotCompileTemplate e -> "There’s something wrong with the template.\n" <> e
    CannotRunBuild e -> "Build error: \n" <> show e
    CannotGetCommitHash e -> "Can’t read latest commit hash: \n" <> show e
    UnsupportedTemplateFormat extension -> "The " <> extension <> " template format is not supported"

getCommitShortHash :: IO Text
getCommitShortHash =
  runCommandWithDir Nothing "git" ["rev-parse", "--short", "HEAD"] >>= \case
    (System.ExitSuccess, hash, _)  -> pure $ toText hash
    (System.ExitFailure _, _, err) -> throw $ CannotGetCommitHash err

getCurrentFormattedDate :: IO Text
getCurrentFormattedDate = toText . formatTime defaultTimeLocale "%F %X" <$> getCurrentTime

runCommandWithDir :: Maybe FilePath -> String -> [String] -> IO (System.ExitCode, String, String)
runCommandWithDir cwd cmd args =
  Proc.readCreateProcessWithExitCode proc ""
  where
    proc = Proc.CreateProcess
      { cmdspec = Proc.RawCommand cmd args
      , cwd = cwd
      , env = Nothing
      , std_in = Proc.Inherit
      , std_out = Proc.Inherit
      , std_err = Proc.Inherit
      , close_fds = False
      , create_group = False
      , delegate_ctlc = True
      , detach_console = False
      , create_new_console = False
      , new_session = False
      , child_group = Nothing
      , child_user = Nothing
      , use_process_jobs = False
      }

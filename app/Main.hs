{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main (main) where

import           Control.Arrow        (left)
import           Control.Exception
import qualified Data.Aeson           as Json
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as Text
import qualified Data.Text.IO         as TIO
import qualified Options.Applicative  as CLI
import qualified System.Directory     as System
import qualified System.Exit          as System
import           System.FilePath      ((<.>), (</>))
import qualified System.FilePath      as FP
import qualified System.IO.Temp       as Temp
import qualified System.Process       as Proc
import qualified Text.DocLayout       as Template
import qualified Text.DocTemplates    as Template
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
  | CannotRunBuild String
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

  Temp.withTempDirectory "." "temp" $ \tmpDir -> do
    fillTemplate format flags >>= TIO.writeFile (tmpDir </> fileName)

    exitCode <-
      case format of
        LaTex -> do
          runCommandWithDir tmpDir "latexmk" ["-xelatex", show fileName]

    case exitCode of
      (System.ExitSuccess, _, _) -> do
        TIO.putStrLn "✍️  Resume created"
        let outputPdf = baseName <.> "pdf"
        System.copyFile (tmpDir </> outputPdf) outputPdf

      (System.ExitFailure _, _, err) -> throw (CannotRunBuild err)


  where
    handler e = TIO.hPutStrLn stderr (Text.pack $ displayException e) >> exitFailure

fillTemplate :: TemplateFormat -> Flags -> IO Text
fillTemplate format Flags {..} = do
    jsonData <- throwLeft $ decodeJsonResume <$> readFileAsByteString flagPathToJson
    compiledTemplate <- throwLeft $ compileTemplate =<< readFileAsText flagPathToTemplate

    pure $ Template.render Nothing $
      Template.renderTemplate compiledTemplate (escapeJson format jsonData)

  where
    throwLeft = fmap (either throw id)

readFileAsByteString :: MonadIO m => FilePath -> m BL.ByteString
readFileAsByteString = liftIO . BL.readFile

readFileAsText :: MonadIO m => FilePath -> m Text
readFileAsText = liftIO . TIO.readFile

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
    CannotRunBuild e -> "Build error: \n" <> e
    UnsupportedTemplateFormat extension -> "The " <> extension <> " template format is not supported"

runCommandWithDir :: FilePath -> String -> [String] -> IO (System.ExitCode, String, String)
runCommandWithDir cwd cmd args =
  Proc.readCreateProcessWithExitCode proc ""
  where
    proc = Proc.CreateProcess
      { cmdspec = Proc.RawCommand cmd args
      , cwd = Just cwd
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

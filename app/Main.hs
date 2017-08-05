{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Aeson (eitherDecode, Value)
import JsonDiff (diffStructures)
import qualified Options.Applicative as Opt
import Options.Applicative (argument, str, metavar, help)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Protolude

data Args = Args
  { fileExpected :: FilePath
  , fileActual :: FilePath
  }

parseArgs :: Opt.Parser Args
parseArgs =
  Args <$> argument str (metavar "FILE1" <> help "The expected JSON") <*>
  argument str (metavar "FILE2" <> help "The actual JSON")

opts :: Opt.ParserInfo Args
opts =
  Opt.info
    (Opt.helper <*> parseArgs)
    (Opt.fullDesc <> Opt.progDesc "Diffs two JSON documents by structure" <>
     Opt.header "json-diff: structural differ")

main :: IO ()
main = do
  Args expected actual <- Opt.execParser opts
  expec <- getJson expected
  act <- getJson actual
  case diffStructures expec act of
    [] -> exitSuccess
    diffs -> exit (renderDiff (pretty diffs))

renderDiff :: Doc ann -> Text
renderDiff = renderStrict . layoutPretty defaultLayoutOptions

getJson :: FilePath -> IO Value
getJson filename = do
  contents <- readFile filename
  case eitherDecode (strConv Lenient contents) of
    Left parseErr -> exit ("Failed to parse " <> filename <> ": " <> parseErr)
    Right json -> return json

exit :: Print a => a -> IO b
exit message = putStrLn message >> exitFailure

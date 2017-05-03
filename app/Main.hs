{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Aeson (eitherDecode)
import GHC.Base (String)
import JsonDiff (JsonDiff, prettyDiff, diffStructures)
import qualified Options.Applicative as Opt
import Options.Applicative (argument, str, metavar, help)
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
  (Args expected actual) <- Opt.execParser opts
  runDiff <$> readFile expected <*> readFile actual >>= \case
    Right [] -> exitSuccess
    Right diffs -> exit (prettyDiff diffs)
    Left err -> exit (strConv Lenient err)

exit :: Text -> IO a
exit message = putText message >> exitFailure

runDiff :: Text -> Text -> Either String [JsonDiff]
runDiff expected actual =
  diffStructures <$> eitherDecode (strConv Lenient expected) <*>
  eitherDecode (strConv Lenient actual)

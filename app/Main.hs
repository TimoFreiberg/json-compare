module Main where

import Data.Aeson (eitherDecode)
import GHC.Base (String)
import JsonDiff (JsonDiff, prettyDiff, diffStructures)
import Options.Applicative as Opt
import Protolude

main :: IO ()
main = do
  args <- getArgs
  case args of
    [f1, f2] -> do
      json1 <- readFile f1
      json2 <- readFile f2
      case runDiff json1 json2 of
        Right diff ->
          case diff of
            [] -> exitSuccess
            diffs@(_:_) -> exit . prettyDiff $ diffs
        Left parseErr -> exit . strConv Lenient $ parseErr
    other ->
      exit
        ("invalid argument " <> show other <> ". Please input two file names")

exit :: Text -> IO b
exit message = putText message >> exitFailure

runDiff :: Text -> Text -> Either String [JsonDiff]
runDiff expected actual =
  diffStructures <$> eitherDecode (strConv Lenient expected) <*>
  eitherDecode (strConv Lenient actual)

module Main where

import Data.Aeson (eitherDecode)
import GHC.Base (String)
import JsonDiff
import Protolude

main :: IO ()
main = do
  args <- getArgs
  case args of
    [f1, f2] -> do
      json1 <- readFile f1
      json2 <- readFile f2
      case runDiff json1 json2 of
        Right diff -> putText . prettyDiff $ diff
        Left parseErr -> putStrLn parseErr
    other ->
      putText
        ("invalid argument " <> show other <> ". Please input two file names")

runDiff :: Text -> Text -> Either String [JsonDiff]
runDiff expected actual =
  diffStructures <$> eitherDecode (strConv Lenient expected) <*>
  eitherDecode (strConv Lenient actual)

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (eitherDecode, Value)
import JsonDiff (diffStructures)
import Options.Applicative (argument, str, metavar, help, Parser, ParserInfo, info, helper, fullDesc, progDesc, header, execParser)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Protolude


parseArgs :: Parser Args
parseArgs =
  Args <$> argument str (metavar "FILE1" <> help "The expected JSON") <*>
  argument str (metavar "FILE2" <> help "The actual JSON")

opts :: ParserInfo Args
opts =
  info
    (helper <*> parseArgs)
    (fullDesc <> progDesc "Diffs two JSON documents by structure" <>
     header "json-diff: structural differ")

main :: IO ()
main = do
  Args expected actual <- execParser opts
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

data Args = Args
  { fileExpected :: FilePath
  , fileActual :: FilePath
  }

{-
GIT_EXTERNAL_DIFF
           When the environment variable GIT_EXTERNAL_DIFF is set, the program named by it is called, instead of the diff invocation described above. For a path that is added, removed,
           or modified, GIT_EXTERNAL_DIFF is called with 7 parameters:

               path old-file old-hex old-mode new-file new-hex new-mode

           where:

       <old|new>-file
           are files GIT_EXTERNAL_DIFF can use to read the contents of <old|new>,

       <old|new>-hex
           are the 40-hexdigit SHA-1 hashes,

       <old|new>-mode
           are the octal representation of the file modes.

           The file parameters can point at the user’s working file (e.g.  new-file in "git-diff-files"), /dev/null (e.g.  old-file when a new file is added), or a temporary file (e.g.
           old-file in the index).  GIT_EXTERNAL_DIFF should not worry about unlinking the temporary file --- it is removed when GIT_EXTERNAL_DIFF exits.

           For a path that is unmerged, GIT_EXTERNAL_DIFF is called with 1 parameter, <path>.

           For each path GIT_EXTERNAL_DIFF is called, two environment variables, GIT_DIFF_PATH_COUNTER and GIT_DIFF_PATH_TOTAL are set.

       GIT_DIFF_PATH_COUNTER
           A 1-based counter incremented by one for every path.

       GIT_DIFF_PATH_TOTAL
           The total number of paths.

-}

module JsonDiff.Args
  ( parseArgs
  , Args(Args)
  ) where

import Options.Applicative
       (Parser, ParserInfo, ParserResult, argument, execParserPure,
        fullDesc, handleParseResult, header, helper, info, metavar, prefs,
        progDesc, str)
import Protolude

data Args =
  Args FilePath
       FilePath

gitStyleArgs :: Parser Args
gitStyleArgs =
  Args <$> (ignoredArg *> file1Arg <* ignoredArg <* ignoredArg) <*>
  (file2Arg <* ignoredArg <* ignoredArg)

twoArgs :: Parser Args
twoArgs = Args <$> file1Arg <*> file2Arg

file1Arg :: Parser FilePath
file1Arg = argument str (metavar "FILE1")

file2Arg :: Parser FilePath
file2Arg = argument str (metavar "FILE2")

ignoredArg :: Parser FilePath
ignoredArg = argument str mempty

opts :: Parser a -> ParserInfo a
opts args =
  info
    (helper <*> args)
    (fullDesc <> progDesc "Diffs two JSON documents by structure" <>
     header "json-diff: structural differ")

runParser :: ParserInfo a -> [FilePath] -> ParserResult a
runParser = execParserPure (prefs mempty)

parseArgs :: [FilePath] -> IO Args
parseArgs args =
  handleParseResult $
  case (length args) of
    2 -> runParser (opts twoArgs) args
    _ -> runParser (opts gitStyleArgs) args

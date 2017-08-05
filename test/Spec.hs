import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import JsonDiff
import Protolude
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

main :: IO ()
main = do
    fileA <- BSL.readFile "test/a.json"
    fileB <- BSL.readFile "test/b.json"
    case (Json.decode fileA, Json.decode fileB) of
        (Just jsonA, Just jsonB) -> do
            expected <- TL.readFile "test/expected.output"
            let diff = diffStructures jsonA jsonB
                renderedDiff = (renderLazy . layoutPretty defaultLayoutOptions . pretty) diff
            case TL.strip renderedDiff == TL.strip expected of
                False -> do
                    putText "Error: actual and expected diff do not match."
                    putText "Actual:"
                    putText (TL.toStrict renderedDiff)
                    putText "Expected:"
                    putText (TL.toStrict expected)
                    exitFailure
                True -> pure ()
        _otherwise -> do
            putText "Uh oh, input isn’t proper JSON"
            exitFailure

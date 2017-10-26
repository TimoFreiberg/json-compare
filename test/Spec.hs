import qualified Data.Aeson as Json
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import JsonDiff
import Protolude

main :: IO ()
main = do
  fileA <- BSL.readFile "test/a.json"
  fileB <- BSL.readFile "test/b.json"
  case (Json.decode fileA, Json.decode fileB) of
    (Just jsonA, Just jsonB) -> do
      putStrLn (encodePretty jsonA)
      putStrLn (encodePretty jsonB)
      let diff = diffStructures jsonA jsonB
      print diff
            -- expected <- TL.readFile "test/expected.output"
            -- let diff = diffStructures jsonA jsonB
            --     renderedDiff = (renderLazy . layoutPretty defaultLayoutOptions . pretty) diff
            -- when (TL.strip renderedDiff /= TL.strip expected) $ do
            --         putText "Error: actual and expected diff do not match."
            --         putText "Actual:"
            --         putText (TL.toStrict renderedDiff)
            --         putText "Expected:"
            --         putText (TL.toStrict expected)
            --         exitFailure
    _otherwise -> do
      putText "Uh oh, input isn’t proper JSON"
      exitFailure

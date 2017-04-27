{-# LANGUAGE ViewPatterns #-}

module JsonDiff where

-- import qualified Control.Monad.State.Strict as State
-- import Control.Monad.State.Strict (modify, get)
import Data.Aeson (Value)
import qualified Data.Aeson as Json
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import Data.Text (unlines)
import qualified Data.Vector as Vector
import Lib.Prelude

data JsonDiff
  = KeyNotPresent Text
  | NotFoundInArray [Value]
                    Value
  | WrongType Value
              Value
  deriving (Show)

prettyDiff :: [JsonDiff] -> Text
prettyDiff = unlines . map singlePretty
  where
    singlePretty :: JsonDiff -> Text
    singlePretty (KeyNotPresent t) = "Key not found: " <> t
    singlePretty (NotFoundInArray arr val) =
      "No structure matching " <> prettyText val <> " found in " <>
      prettyText arr
    singlePretty (WrongType expected actual) =
      "Wrong type, expected: " <> prettyText expected <> ", actual: " <>
      prettyText actual
    prettyText
      :: ToJSON a
      => a -> Text
    prettyText = strConv Lenient . encodePretty

-- | @diffStructures expected actual@ compares the structures of the two JSON values and reports each item in @actual@ that is not present in @expected@
-- if @actual@ is a strict subset (or sub-object) of @expected@, the list should be null
--
diffStructures
  :: Value -- ^ expected
  -> Value -- ^ actual
  -> [JsonDiff] -- ^ differences from actual to expected
diffStructures _ Json.Null = []
diffStructures (Json.Bool _) (Json.Bool _) = []
diffStructures (Json.Number _) (Json.Number _) = []
diffStructures (Json.String _) (Json.String _) = []
diffStructures (Json.Object expected) (Json.Object actual) =
  snd $ foldr objDiffStep (expected, []) (Map.toList actual)
diffStructures (Json.Array expected) (Json.Array actual) =
  snd $ foldr arrayDiffStep (Vector.toList expected, []) (actual)
diffStructures expected actual = [WrongType expected actual]

diff expected actual = evalState (runDiff expected actual) ([], [])

runDiff :: Value -> Value -> State [JsonDiff]
runDiff expected actual =
  if sameShape expected actual
    then undefined -- DFS into the structures
    else do
      (path, diff) <- get
      return (diff ++ [WrongType expected actual]) -- add path info here

sameShape :: Value -> Value -> Bool
sameShape Json.Null Json.Null = True
sameShape (Json.Bool _) (Json.Bool _) = True
sameShape (Json.Number _) (Json.Number _) = True
sameShape (Json.String _) (Json.String _) = True
sameShape (Json.Object _) (Json.Object _) = True
sameShape (Json.Array _) (Json.Array _) = True
sameShape _ _ = False

objDiffStep
  :: (Text, Value)
  -> (HashMap Text Value, [JsonDiff])
  -> (HashMap Text Value, [JsonDiff])
objDiffStep (k, vActual) (expected1, diffs) =
  case Map.lookup k expected1 of
    Just vExpected ->
      (Map.delete k expected1, diffs ++ diffStructures vExpected vActual)
    Nothing -> (expected1, diffs ++ [KeyNotPresent k])

arrayDiffStep :: Value -> ([Value], [JsonDiff]) -> ([Value], [JsonDiff])
arrayDiffStep actual (expected, diffs) =
  case filter (sameShape actual) expected of
    [] -> (expected, diffs ++ [NotFoundInArray expected actual])
    xs -> (expected, diffs ++ concatMap (\x -> diffStructures x actual) xs)

obj1 :: Value
obj1 =
  (\(Just x) -> x) $
  Json.decode
    "[{\"serviceId\": 0, \"serviceName\": \"string\", \"requiredServiceDataItems\": [{\"profileDataFieldItem\": {\"fieldOwnerType\": \"ACCOUNT\", \"itemId\": \"USER_ID\", \"profileDataFieldRelationshipType\": \"GROUP\", \"childrenIds\": [\"USER_ID\"]}, \"profileDataFieldGroup\": {\"itemId\": \"string\", \"profileDataFieldRelationshipType\": \"GROUP\", \"childrenIds\": [\"USER_ID\"]}}], \"requiredCustomProperties\": [\"string\"]}]"

obj2 :: Value
obj2 =
  (\(Just x) -> x) $
  Json.decode
    "[{\"serviceId\": 0, \"serviceName\": \"string\", \"requiredServiceDataItems\": [{\"profileDataFieldItem\": {\"fieldOwnerType\": \"ACCOUNT\", \"itemId\": \"USER_ID\", \"profileDataFieldRelationshipType\": \"GROUP\", \"childrenIds\": [\"USER_ID\", \"shouldNotBeFound\"]}, \"profileDataFieldGroup\": {\"itemId\": \"string\", \"profileDataFieldRelationshipType\": {\"test\": 15}, \"childrenIds\": [\"USER_ID\"]}}], \"requiredCustomProperties\": [15]}]"

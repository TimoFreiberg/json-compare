{-# LANGUAGE ViewPatterns #-}

module JsonDiff
  ( diffStructures
  ) where

import Data.Aeson (Value)
import qualified Data.Aeson as Json
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as Text
import Lib.Prelude

type JsonPath = [JsonPathStep]

data JsonPathStep
  = Root
  | Key Text
  | Ix Int
  deriving (Show)

data JsonDiff
  = KeyNotPresent JsonPath
                  Text
  | NotFoundInArray JsonPath
                    [Value]
                    Value
  | WrongType JsonPath
              Value
              Value
  deriving (Show)

test :: IO ()
test = putText $prettyDiff $ diffStructures obj1 obj2

prettyDiff :: [JsonDiff] -> Text
prettyDiff = Text.unlines . map singlePretty
  where
    singlePretty :: JsonDiff -> Text
    singlePretty (KeyNotPresent p t) = "Key not found: " <> t <> prettyPath p
    singlePretty (NotFoundInArray p arr val) =
      "No structure matching " <> prettyText val <> " found in " <>
      prettyText arr <>
      prettyPath p
    singlePretty (WrongType p expected actual) =
      "Wrong type, expected: " <> prettyText expected <> ", actual: " <>
      prettyText actual <>
      prettyPath p
    prettyPath :: JsonPath -> Text
    prettyPath p =
      "\nFound at path: " <>
      (Text.concat . intersperse "." $ map prettyStep (reverse p))
    prettyStep :: JsonPathStep -> Text
    prettyStep Root = "$"
    prettyStep (Key k) = k
    prettyStep (Ix i) = show i
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
diffStructures expected actual =
  runReader (diffStructureWithPath expected actual) [Root]

diffStructureWithPath :: Value -> Value -> Reader JsonPath [JsonDiff]
diffStructureWithPath _ Json.Null = return []
    -- null is a valid subset of any JSON
diffStructureWithPath (Json.Bool _) (Json.Bool _) = return []
diffStructureWithPath (Json.Number _) (Json.Number _) = return []
diffStructureWithPath (Json.String _) (Json.String _) = return []
diffStructureWithPath (Json.Object expected) (Json.Object actual) =
  let pairs :: [(Text, Value)]
      pairs = Map.toList actual
      f :: (Text, Value) -> Reader JsonPath [JsonDiff]
      f pair@(k, _) = local ((Key k) :) (objDiffStep expected pair)
  in concat <$> traverse f pairs
diffStructureWithPath (Json.Array expected) (Json.Array actual) =
  concat <$>
  traverse (arrayDiffStep (toList expected)) (zip [0 :: Int ..] $ toList actual)
diffStructureWithPath a b = ask >>= \path -> return [WrongType path a b]

objDiffStep :: HashMap Text Value -> (Text, Value) -> Reader JsonPath [JsonDiff]
objDiffStep expected (k, vActual) =
  case Map.lookup k expected of
    Just vExpected -> diffStructureWithPath vExpected vActual
    Nothing -> ask >>= \path -> return [KeyNotPresent path k]

arrayDiffStep :: [Value] -> (Int, Value) -> Reader JsonPath [JsonDiff]
arrayDiffStep expected (n, actual) =
  local ((Ix n) :) $
  case filter (sameShape actual) expected of
    [] -> ask >>= \path -> return [NotFoundInArray path expected actual]
    xs -> concat <$> traverse (`diffStructureWithPath` actual) xs
      -- FIXME return differences of object with least differences

sameShape :: Value -> Value -> Bool
sameShape Json.Null Json.Null = True
sameShape (Json.Bool _) (Json.Bool _) = True
sameShape (Json.Number _) (Json.Number _) = True
sameShape (Json.String _) (Json.String _) = True
sameShape (Json.Object _) (Json.Object _) = True
sameShape (Json.Array _) (Json.Array _) = True
sameShape _ _ = False

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

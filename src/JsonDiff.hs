{-# LANGUAGE ViewPatterns #-}

module JsonDiff
  ( diffStructures
  , prettyDiff
  , JsonDiff
  ) where

import Data.Aeson (Value)
import qualified Data.Aeson as Json
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as Text
import Protolude

type JsonPath = [JsonPathStep]

data JsonPathStep
  = Root -- ^ Root of the JSON document
  | Key Text -- ^ Key of an object
  | Ix Int -- ^ Index of an array
  deriving (Show)

data JsonDiff
  = KeyNotPresent JsonPath -- ^ the path to the object
                  Text -- ^ the key that was not found
  | NotFoundInArray JsonPath -- ^ the path to the array
                    [Value] -- ^ the elements in the array
                    Value -- ^ the element that was not found
  | WrongType JsonPath -- ^ the path to the JSON value
              Value -- ^ the expected value
              Value -- ^ the actual value
  deriving (Show)

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
    prettyText :: ToJSON a => a -> Text
    prettyText = strConv Lenient . encodePretty

-- | @diffStructures expected actual@ compares the structures of the two JSON values and reports each item in @actual@ that is not present in @expected@
-- if @actual@ is a strict subset (or sub-object) of @expected@, the list should be null
--
diffStructures ::
     Value -- ^ expected
  -> Value -- ^ actual
  -> [JsonDiff] -- ^ differences from actual to expected
diffStructures expected actual = diffStructureAtPath [Root] expected actual

diffStructureAtPath :: JsonPath -> Value -> Value -> [JsonDiff]
diffStructureAtPath _ _ Json.Null = []
    -- null is a valid subset of any JSON
diffStructureAtPath _ (Json.Bool _) (Json.Bool _) = []
diffStructureAtPath _ (Json.Number _) (Json.Number _) = []
diffStructureAtPath _ (Json.String _) (Json.String _) = []
diffStructureAtPath path (Json.Object expected) (Json.Object actual) =
  concatMap (diffObjectWithEntry path expected) (Map.toList actual)
diffStructureAtPath path (Json.Array expected) (Json.Array actual) =
  concatMap
    (diffArrayWithElement path (toList expected))
    (zip [0 :: Int ..] $ toList actual)
diffStructureAtPath path a b = [WrongType path a b]

diffObjectWithEntry ::
     JsonPath -> HashMap Text Value -> (Text, Value) -> [JsonDiff]
diffObjectWithEntry path expected (k, vActual) =
  case Map.lookup k expected of
    Just vExpected -> diffStructureAtPath newPath vExpected vActual
    Nothing -> [KeyNotPresent newPath k]
  where
    newPath = Key k : path

diffArrayWithElement :: JsonPath -> [Value] -> (Int, Value) -> [JsonDiff]
diffArrayWithElement path expected (n, actual) =
  case filter (sameType actual) expected of
    [] -> [NotFoundInArray newPath expected actual]
    xs ->
      minimumBy (comparing length) $
      map (\x -> diffStructureAtPath newPath x actual) xs
  where
    newPath = Ix n : path

sameType :: Value -> Value -> Bool
sameType Json.Null Json.Null = True
sameType (Json.Bool _) (Json.Bool _) = True
sameType (Json.Number _) (Json.Number _) = True
sameType (Json.String _) (Json.String _) = True
sameType (Json.Object _) (Json.Object _) = True
sameType (Json.Array _) (Json.Array _) = True
sameType _ _ = False

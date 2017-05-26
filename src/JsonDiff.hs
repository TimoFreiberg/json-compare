{-# LANGUAGE ViewPatterns #-}

module JsonDiff
  ( diffStructures
  , prettyDiff
  , JsonDiff
  ) where

import Data.Aeson (Value)
import qualified Data.Aeson as Json
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
                    JsonType -- ^ the type of the element that was not found
  | WrongType JsonPath -- ^ the path to the JSON value
              JsonType -- ^ the type of the expected value
              JsonType -- ^ the type of the actual value
  deriving (Show)

data JsonType
  = Null
  | Bool
  | Number
  | String
  | Object
  | Array
  deriving (Eq, Enum, Bounded, Show)

prettyDiff :: [JsonDiff] -> Text
prettyDiff = Text.unlines . map singlePretty
  where
    singlePretty :: JsonDiff -> Text
    singlePretty (KeyNotPresent p k) = "No key " <> show k <> prettyPath p
    singlePretty (NotFoundInArray p t) =
      "No value with type " <> show t <> prettyPath p
    singlePretty (WrongType p expected actual) =
      "Wrong type, expected: " <> show expected <> ", actual: " <> show actual <>
      prettyPath p
    prettyPath :: JsonPath -> Text
    prettyPath p =
      "\n  Found at path: " <>
      (Text.concat . intersperse "." $ map prettyStep (reverse p))
    prettyStep :: JsonPathStep -> Text
    prettyStep Root = "$"
    prettyStep (Key k) = k
    prettyStep (Ix i) = show i

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
diffStructureAtPath path a b = [WrongType path (toType a) (toType b)]

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
    [] -> [NotFoundInArray newPath (toType actual)]
    xs ->
      minimumBy (comparing length) $
      map (\x -> diffStructureAtPath newPath x actual) xs
  where
    newPath = Ix n : path

sameType :: Value -> Value -> Bool
sameType = (==) `on` toType

toType :: Value -> JsonType
toType Json.Null = Null
toType (Json.Bool _) = Bool
toType (Json.Number _) = Number
toType (Json.String _) = String
toType (Json.Object _) = Object
toType (Json.Array _) = Array

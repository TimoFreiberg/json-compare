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
diffStructures expected actual = diffStructureWithPath [Root] expected actual

diffStructureWithPath :: JsonPath -> Value -> Value -> [JsonDiff]
diffStructureWithPath _ _ Json.Null = []
    -- null is a valid subset of any JSON
diffStructureWithPath _ (Json.Bool _) (Json.Bool _) = []
diffStructureWithPath _ (Json.Number _) (Json.Number _) = []
diffStructureWithPath _ (Json.String _) (Json.String _) = []
diffStructureWithPath path (Json.Object expected) (Json.Object actual) =
  concatMap (objDiffStep path expected) (Map.toList actual)
diffStructureWithPath path (Json.Array expected) (Json.Array actual) =
  concatMap
    (arrayDiffStep path (toList expected))
    (zip [0 :: Int ..] $ toList actual)
diffStructureWithPath path a b = [WrongType path a b]

objDiffStep :: JsonPath -> HashMap Text Value -> (Text, Value) -> [JsonDiff]
objDiffStep path expected (k, vActual) =
  case Map.lookup k expected of
    Just vExpected -> diffStructureWithPath newPath vExpected vActual
    Nothing -> [KeyNotPresent newPath k]
  where
    newPath = Key k : path

arrayDiffStep :: JsonPath -> [Value] -> (Int, Value) -> [JsonDiff]
arrayDiffStep path expected (n, actual) =
  case filter (sameShape actual) expected of
    [] -> [NotFoundInArray newPath expected actual]
    xs -> concat $ traverse (\x -> diffStructureWithPath newPath x actual) xs
  where
    newPath = Ix n : path
      -- FIXME return differences of object with least differences

sameShape :: Value -> Value -> Bool
sameShape Json.Null Json.Null = True
sameShape (Json.Bool _) (Json.Bool _) = True
sameShape (Json.Number _) (Json.Number _) = True
sameShape (Json.String _) (Json.String _) = True
sameShape (Json.Object _) (Json.Object _) = True
sameShape (Json.Array _) (Json.Array _) = True
sameShape _ _ = False

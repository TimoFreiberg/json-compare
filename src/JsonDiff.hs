{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module JsonDiff
  ( diffStructures
  , JsonDiff
  ) where

import Data.Aeson (Value)
import qualified Data.Aeson as Json
import qualified Data.Algorithm.Diff as Diff
import Data.Algorithm.Diff (Diff)
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import Data.Text.Prettyprint.Doc

import Protolude

type JsonPath = [JsonPathStep]

data JsonPathStep
  = Root -- ^ Root of the JSON document
  | Key Text -- ^ Key of an object
  | Ix Int -- ^ Index of an array
  deriving (Show)

data JsonDiff
  = MapKey Text -- ^ the key that changed
  | ArrayIx Int -- ^ the index where something changed
  | Type JsonType -- ^ the type that changed
  deriving (Show)

data JsonType
  = Null
  | Bool
  | Number
  | String
  | Object
  | Array
  deriving (Eq, Enum, Bounded, Show)

-- | @diffStructures expected actual@ compares the structures of the two JSON values and reports each item in @actual@ that is not present in @expected@
-- if @actual@ is a strict subset (or sub-object) of @expected@, the list should be null
--
diffStructures ::
     Value -- ^ expected
  -> Value -- ^ actual
  -> [Diff JsonDiff] -- ^ differences from actual to expected
diffStructures expected actual = diffStructureAtPath [Root] expected actual

diffStructureAtPath :: JsonPath -> Value -> Value -> [Diff JsonDiff]
diffStructureAtPath p a b
  | not $ sameType a b = change (Type $ toType a) (Type $ toType b)
  | both isObject a b = 
diffStructureAtPath _ (Json.Bool _) (Json.Bool _) = []
diffStructureAtPath _ (Json.Number _) (Json.Number _) = []
diffStructureAtPath _ (Json.String _) (Json.String _) = []
diffStructureAtPath path (Json.Object expected) (Json.Object actual) =
  concatMap (diffObjectWithEntry path expected) (Map.toList actual)
diffStructureAtPath path (Json.Array expected) (Json.Array actual) =
  concatMap (diffArrayWithElement path (toList expected)) (toIndexedList actual)
diffStructureAtPath path a b = [WrongType path (toType a) (toType b)]

diffObjectWithEntry ::
     JsonPath -> HashMap Text Value -> (Text, Value) -> [JsonDiff]
diffObjectWithEntry path expected (k, vActual) =
  case Map.lookup k expected of
    Just vExpected -> diffStructureAtPath (Key k : path) vExpected vActual
    Nothing -> [KeyNotPresent path k]

diffArrayWithElement :: JsonPath -> [Value] -> (Int, Value) -> [JsonDiff]
diffArrayWithElement path expected (n, actual) =
  case filter (sameType actual) expected of
    [] -> [NotFoundInArray path n (toType actual)]
    xs ->
      minimumBy (comparing length) $
      map (\x -> diffStructureAtPath (Ix n : path) x actual) xs

toIndexedList :: Foldable l => l a -> [(Int, a)]
toIndexedList = zip [0 ..] . toList

change a b = [Diff.First a, Diff.Second b]

both pred a b = pred a && pred b
isObject = (== Object) . toType

sameType :: Value -> Value -> Bool
sameType = (==) `on` toType

toType :: Value -> JsonType
toType Json.Null = Null
toType (Json.Bool _) = Bool
toType (Json.Number _) = Number
toType (Json.String _) = String
toType (Json.Object _) = Object
toType (Json.Array _) = Array

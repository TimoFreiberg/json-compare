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
  -> [(JsonPath, Diff JsonDiff)] -- ^ differences from actual to expected
diffStructures expected actual = diffStructureAtPath [Root] expected actual

diffStructureAtPath :: JsonPath -> Value -> Value -> [(JsonPath, Diff JsonDiff)]
diffStructureAtPath p a b
  | not $ sameType a b = change p (Type $ toType a) (Type $ toType b)
diffStructureAtPath p (Json.Object a) (Json.Object b) =
  concatMap diffAtKey keyDiffs
  where
    keyDiffs = Diff.getDiff (sort $ Map.keys a) (sort $ Map.keys b)
    diffAtKey (Diff.Both k1 k2) =
      diffStructureAtPath (Key k1 : p) (a Map.! k1) (b Map.! k2)
    diffAtKey (Diff.First k) = [(Key k : p, Diff.First $ MapKey k)]
    diffAtKey (Diff.Second k) = [(Key k : p, Diff.Second $ MapKey k)]
diffStructureAtPath p (Json.Array a) (Json.Array b) = []
  --   zip (map ((: p) . Ix) [0..]) $ map (mapDiff snd) $
  -- Diff.getDiffBy (\(i,v1) (_, v2) -> null $ diffStructureAtPath (Ix i : p) v1 v2) (toIndexedList a) (toIndexedList b)
diffStructureAtPath p a b
  | sameType a b = [(p, Diff.Both (typ a) (typ b))]
  where
    typ = Type . toType

mapDiff f (Diff.First a) = Diff.First (f a)
mapDiff f (Diff.Second a) = Diff.Second (f a)
mapDiff f (Diff.Both a1 a2) = Diff.Both (f a1) (f a2)

toIndexedList :: Foldable l => l a -> [(Int, a)]
toIndexedList = zip [0 ..] . toList

change :: JsonPath -> a -> a -> [(JsonPath, Diff a)]
change p a b = [(p, Diff.First a), (p, Diff.Second b)]

sameType :: Value -> Value -> Bool
sameType = (==) `on` toType

toType :: Value -> JsonType
toType Json.Null = Null
toType (Json.Bool _) = Bool
toType (Json.Number _) = Number
toType (Json.String _) = String
toType (Json.Object _) = Object
toType (Json.Array _) = Array

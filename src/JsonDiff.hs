{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module JsonDiff
  ( diffStructures
  , JsonDiff
  ) where

import Data.Aeson (Value)
import qualified Data.Aeson as Json
import qualified Data.Algorithm.Diff as Diff
import qualified Data.HashMap.Strict as Map
import Data.List(groupBy)
import Data.Text.Prettyprint.Doc

import Protolude

type JsonPath = [JsonPathStep]

data JsonPathStep
  = Root -- ^ Root of the JSON document
  | Key Text -- ^ Key of an object
  | Ix Int -- ^ Index of an array
  deriving (Show, Eq, Ord)

instance Pretty JsonPathStep where
  pretty step = case step of
    Root  -> "$"
    Key k -> pretty k
    Ix i  -> pretty i
  prettyList = prettyPath

data JsonDiff = JsonDiff
  { jsonPath :: JsonPath
  , diff :: (Diff JsonStructurePart)
  }

prettyPath :: [JsonPathStep] -> Doc ann
prettyPath = concatWith (surround ".") . map pretty . reverse

instance Pretty JsonDiff where
    pretty (JsonDiff p d) = vsep [pretty p <> ":", pretty d]
    prettyList = vsep . map ((<> line) . prettyDiffGroup) . groupBy ((==) `on` jsonPath) . sortBy (comparing jsonPath)
      where
        prettyDiffGroup [] = emptyDoc
        prettyDiffGroup xs@(x:_) = vsep [pretty (jsonPath x) <> ":", indent 2 (vsep (map (pretty . diff) xs))]

data Diff a
  = Old a
  | New a
  deriving (Show, Eq)

instance Pretty a => Pretty (Diff a) where
  pretty (Old a) = "-" <+> pretty a
  pretty (New a) = "+" <+> pretty a

data JsonStructurePart
  = MapKey Text -- ^ the key that changed
  | Type JsonType -- ^ the type that changed
  deriving (Show)

instance Pretty JsonStructurePart where
  pretty (MapKey k) =  dquotes (pretty k)
  pretty (Type t) = pretty t

data JsonType
  = Null
  | Bool
  | Number
  | String
  | Object
  | Array
  deriving (Eq, Enum, Bounded, Show)
instance Pretty JsonType

-- | @diffStructures old new@ compares the structures of the two JSON values and
-- returns each structural difference with the path at which the difference occurred
diffStructures ::
     Value
  -> Value
  -> [JsonDiff]
diffStructures = diffStructureAtPath [Root]

diffStructureAtPath :: JsonPath -> Value -> Value -> [JsonDiff]
diffStructureAtPath p a b
  | not $ sameType a b = change p (Type $ toType a) (Type $ toType b)
diffStructureAtPath p (Json.Object a) (Json.Object b) =
  concatMap diffAtKey keyDiffs
  where
    keyDiffs = Diff.getDiff (sort $ Map.keys a) (sort $ Map.keys b)
    diffAtKey (Diff.Both k1 k2) =
      diffStructureAtPath (Key k1 : p) (a Map.! k1) (b Map.! k2)
    diffAtKey (Diff.First k) = [JsonDiff (Key k : p) (Old $ MapKey k)]
    diffAtKey (Diff.Second k) = [JsonDiff (Key k : p) (New $ MapKey k)]
diffStructureAtPath p (Json.Array a) (Json.Array b) = []
  --   zip (map ((: p) . Ix) [0..]) $ map (mapDiff snd) $
  -- Diff.getDiffBy (\(i,v1) (_, v2) -> null $ diffStructureAtPath (Ix i : p) v1 v2) (toIndexedList a) (toIndexedList b)
diffStructureAtPath _ _ _ = []

change :: JsonPath -> JsonStructurePart -> JsonStructurePart -> [JsonDiff]
change p a b = [JsonDiff p (Old a), JsonDiff p (New b)]

sameType :: Value -> Value -> Bool
sameType = (==) `on` toType

toType :: Value -> JsonType
toType Json.Null = Null
toType (Json.Bool _) = Bool
toType (Json.Number _) = Number
toType (Json.String _) = String
toType (Json.Object _) = Object
toType (Json.Array _) = Array

# Diffing JSON schemas to find API mismatches early

While [we don't want to generate our client code from the API documentation](https://blog.novatec-gmbh.de/the-problems-with-swagger/),
doing it by hand leaves room for error.
When (and it's always when, not if) such an error occurs, it can take until a while after delivery that the devs notice.
This usually means hotfixing, and therefore making sure that this time, the message structure was implemented correctly, with human eyes, under time pressure.

Since this is obviously a recipe for disaster, I wrote a tool to diff JSON structure for me.
I used [Haskell](https://www.haskell.org), my favorite functional programming language.

## The problem

I needed to verify that the JSON structure our application accepts is a valid subset of the JSON structure that actually sent from the other system.

That simply means that the data we read must exist in the message we actually get, but it's okay if we ignore some fields.

Structural differences are possible at object keys and array elements.
Additionally, JSON values at the same path might have a different type.

## The solution, with code

I represented the possible structural differences with the following [data type](https://en.wikipedia.org/wiki/Algebraic_data_type):

```haskell
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
```

These data structures represent the entire result of the structural diff:

1. where the diff was found
2. the type of mismatch (via the constructor)
3. the mismatched values

The path is defined like this:

```haskell
data JsonPathStep
  = Root -- ^ Root of the JSON document
  | Key Text -- ^ Key of an object
  | Ix Int -- ^ Index of an array
  deriving (Show)

type JsonPath = [JsonPathStep]
```

The actual diffing is done by the following function:

```haskell
diffStructures
  :: Value -- ^ expected/what we want to match
  -> Value -- ^ actual/what we implemented
  -> [JsonDiff] -- ^ differences from actual to expected
diffStructures expected actual = diffStructureWithPath [Root] expected actual
```

If you're unfamiliar with Haskell syntax, `diffStructures` is a function that takes a `Value`, then another `Value` and returns a list of `JsonDiff`s

The actual algorithm is implemented in a private helper function which takes the path as an additional parameter.
The path always starts with `Root`.

```haskell
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
```

## example

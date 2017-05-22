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

The complete code is available on [my github](https://github.com/TimoFreiberg/json-compare).

#### Types

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

1. the path where the diff was found
2. the type of mismatch (encoded via the constructor)
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
diffStructures expected actual = diffStructureAtPath [Root] expected actual
```

If you're unfamiliar with Haskell syntax, `diffStructures` is a function that takes a `Value`, then another `Value` and returns a list of `JsonDiff`s

The algorithm always compares the JSON elements at the same path, starting at the `Root`.
If our `actual` is equal to or a subset of the `expected`, there's no diff.

As long as we make sure that we take the same path in both `expected` and `actual`, the algorithm basically writes itself:

```haskell
diffStructureAtPath :: JsonPath -> Value -> Value -> [JsonDiff]
diffStructureAtPath _ _ Json.Null = []
    -- null is a valid subset of any JSON
```

The first match is trivial - a null value is a subset of anything, so there's no diff.

```haskell
diffStructureAtPath _ (Json.Bool _) (Json.Bool _) = []
diffStructureAtPath _ (Json.Number _) (Json.Number _) = []
diffStructureAtPath _ (Json.String _) (Json.String _) = []
```

Then we match on the three value types (`Bool`s, `Number`s and `String`s) on both sides.
These definitions only match if both JSON values at the same path are both a `Bool`, a `Number` or a `String`.

Since we only want to compare the structure, we ignore the actual value (with the `_` identifier) and always return an empty diff.

```haskell
diffStructureAtPath path (Json.Object expected) (Json.Object actual) =
  concatMap (diffObjectWithEntry path expected) (Map.toList actual)
```

This is the first interesting case: comparing two objects.
We apply the helper function `diffObjectWithEntry` to each key-value pair in the actual map, taking the current path and the expected map with us.

The diff of both objects is the sum of the diffs at each key of the actual object.

```haskell
diffObjectWithEntry :: JsonPath -> HashMap Text Value -> (Text, Value) -> [JsonDiff]
diffObjectWithEntry path expected (k, vActual) =
  case Map.lookup k expected of
    Just vExpected -> diffStructureAtPath newPath vExpected vActual
    Nothing -> [KeyNotPresent newPath k]
  where
    newPath = Key k : path
```

If the key is not present, we return the `KeyNotPresent` diff.

If the key is present, we enter the main diff function again, comparing the values at the key.

```haskell
diffStructureAtPath path (Json.Array expected) (Json.Array actual) =
  concatMap
    (diffArrayWithElement path (toList expected))
    (zip [0 :: Int ..] $ toList actual)
```

Comparing two arrays is implemented similarly.

We diff each element from the actual array with the entire expected array.

To keep track of the path we're exploring, we index each element with `zip [0..]`<sup id="bZip">[1](#fnZip)</sup>

```haskell
diffArrayWithElement :: JsonPath -> [Value] -> (Int, Value) -> [JsonDiff]
diffArrayWithElement path expected (n, actual) =
  case filter (sameType actual) expected of
    [] -> [NotFoundInArray newPath expected actual]
    xs ->
      minimumBy (comparing length) $
      map (\x -> diffStructureWithPath newPath x actual) xs
  where
    newPath = Ix n : path
```

This time, we try to find elements in the expected array that have the same type as the actual.

If none are found, the `NotFoundInArray` diff is returned.

If some are found, we return the smallest diff.
(Because if one element matches perfectly, we don't want to hear about the fifty others that don't)

#### Aside: JSON arrays as lists or as tuples

The way we diff arrays interprets them as (usually) homogenous lists.
This makes the most sense in our case, because we work in a Java environment.
Java doesn't have tuples and it is unidiomatic to use heterogenous lists.

JSON arrays can also be used as tuples, in which case it would be wrong to compare elements at different indices.
Instead, it would require diffing arrays pairwise.




```haskell
diffStructureAtPath path a b = [WrongType path a b]
```



## Example usage

## Footnotes
<b id="fnZip">1</b> The expression `zip [0..] ["foo","bar","baz"]` evaluates to `[(0,"foo"),(1,"bar"),(2,"baz")]`. Thanks to lazy evaluation, we can use the infinite list [0..] for this purpose. [↩](#bZip)


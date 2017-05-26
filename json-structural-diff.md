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
                    JsonType -- ^ the type that was not found
  | WrongType JsonPath -- ^ the path to the JSON value
              JsonType -- ^ the type of the expected value
              JsonType -- ^ the type of the actual value
  deriving (Show)
```

The path is defined like this:

```haskell
data JsonPathStep
  = Root -- ^ Root of the JSON document
  | Key Text -- ^ Key of an object
  | Ix Int -- ^ Index of an array
  deriving (Show)

type JsonPath = [JsonPathStep]
```

#### The algorithm

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

As long as we make sure that we take the same path in both `expected` and `actual`, most of the algorithm writes itself:

```haskell
diffStructureAtPath :: JsonPath -> Value -> Value -> [JsonDiff]
diffStructureAtPath _ _ Json.Null = []
    -- null is a valid subset of any JSON
```

The first case is trivial - a null value is a subset of anything, so there's no diff.

```haskell
diffStructureAtPath _ (Json.Bool _) (Json.Bool _) = []
diffStructureAtPath _ (Json.Number _) (Json.Number _) = []
diffStructureAtPath _ (Json.String _) (Json.String _) = []
```

Then we match on the three value types (`Bool`s, `Number`s and `String`s) on both sides.
These definitions only match if both JSON values at the same path are, for example, `Bool`s.

Since we only want to compare the structure, we ignore the value (with the `_` identifier) and return an empty diff.

```haskell
diffStructureAtPath path (Json.Object expected) (Json.Object actual) =
  concatMap (diffObjectWithEntry path expected) (Map.toList actual)

diffObjectWithEntry ::
     JsonPath -> HashMap Text Value -> (Text, Value) -> [JsonDiff]
diffObjectWithEntry path expected (k, vActual) =
  case Map.lookup k expected of
    Just vExpected -> diffStructureAtPath (Key k : path) vExpected vActual
    Nothing -> [KeyNotPresent path k]
```

This is the first interesting case: comparing two objects.
We define the diff of both objects as the sum of the diffs at each key of the actual object.
This is implemented by applying the helper function `diffObjectWithEntry` to each key-value pair in the actual map (which also takes the current path and the expected map) and concatenating all returned diffs.

Inside the helper, we return the `KeyNotPresent` diff if the key is not present.
Else, we enter the main diff function again, comparing the values at the key.

This case has the following properties:

1. Diffing any object with the empty object will return no diff.
2. Diffing any object with itself will return no diff.
3. Diffing any object with a copy that has a key `k` added will return the diff `KeyNotPresent $.k "k"`

These properties can be used to verify the algorithm with a property-based testing library such as [QuickCheck](https://hackage.haskell.org/package/QuickCheck).


```haskell
diffStructureAtPath path (Json.Array expected) (Json.Array actual) =
  concatMap
    (diffArrayWithElement path (toList expected))
    (toIndexedList actual)

diffArrayWithElement :: JsonPath -> [Value] -> (Int, Value) -> [JsonDiff]
diffArrayWithElement path expected (n, actual) =
  case filter (sameType actual) expected of
    [] -> [NotFoundInArray newPath (toType actual)]
    xs ->
      minimumBy (comparing length) $
      map (\x -> diffStructureAtPath newPath x actual) xs
  where
    newPath = Ix n : path
```

Comparing two arrays is implemented similarly.
We diff each element from the actual array with the entire expected array.

This time, we try to find elements in the expected array that have the same type as the actual.
If none are found, the `NotFoundInArray` diff is returned.
Else, we calculate all diffs and return the smallest.
(Because if one element matches perfectly, we don't want to hear about the fifty others that don't)


```haskell
diffStructureAtPath path a b = [WrongType path (toType a) (toType b)]
```

The last case handles matches any other combination of parameters.
Since all cases where both parameters have the same type were handled before, this case returns the `WrongType` diff.


#### Aside: JSON arrays as lists or as tuples

The way we diff arrays interprets them as homogenous lists.
This makes the most sense in our case, because we work in a Java environment.
Java doesn't have tuples and it is unidiomatic to use heterogenous lists.

JSON arrays can also be used as tuples, in which case it would be wrong to compare elements at different indices.
Instead, it would require diffing arrays pairwise.

A possible extension to this tool would be offer the option to handle arrays as tuples.



## Example usage

Let's try out some simple cases!

```haskell
> let (Just expected) = decode "{}"
> let (Just actual) = decode "{\"key\":[]}"
> diffStructures expected actual
[KeyNotPresent [Key "key",Root] "key"]
```

Of course, we need [a little more code](https://github.com/TimoFreiberg/json-compare/blob/master/src/JsonDiff.hs) to prettyprint the diffs.

Lets try that again:


```haskell
> printDiff expected actual
Expected key: "key"
  At path: $
```

Some more simple cases, using a more readable notation:

```haskell
printDiff
  {"key":0}
  {"key":"text"}

Expected type: Number, actual: String
  At path: $.key
  
printDiff
  [1,2]
  ["foo","bar"]

Expected type: String for index: 0
  At path: $
Expected type: String for index: 1
  At path: $
  
printDiff
  "foo"
  true

Expected type: String, actual: Bool
  At path: $

```

And we can also test the path handling.

```haskell
printDiff
  {
    "k1": [
      {
        "k2": [
          "nested"
        ]
      }
    ]
  }
  {
    "k1": [
      {
        "k2": [
          {}
        ]
      }
    ]
  }

Expected type: Object for index: 0
  At path: $.k1.0.k2

```



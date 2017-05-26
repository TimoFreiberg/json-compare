# json-compare

Compares two JSON documents by structure.
Finds elements in the json document in FILE2 that are not contained in the json document in FILE1.

If FILE2 is equal to FILE1 or a subset of FILE1, then no diff will be returned.

## Installation

Use the [stack build tool](https://docs.haskellstack.org/en/stable/README/).

```bash
git clone https://github.com/TimoFreiberg/json-compare.git
cd json-compare
stack install
```

## Usage

```bash
json-compare expected.json actual.json
```

## TODO

Add flags to modify verbosity.

## Examples

```bash
$ cat expected.json
```
```json
{
  "foo": "bar",
  "baz": 42,
  "quux": [
    1,
    2
  ]
}
```

```bash
$ cat actual.json
```
```json
{
  "notThere": "doesn't matter",
  "foo": 42,
  "baz": "bar",
  "quux": {
    "x": [
      1
    ]
  }
}
```

```bash
$ json-compare expected.json actual.json

No key "notThere"
  Found at path: $.notThere
Wrong type, expected: String, actual: Number
  Found at path: $.foo
Wrong type, expected: Number, actual: String
  Found at path: $.baz
Wrong type, expected: Array, actual: Object
  Found at path: $.quux
```

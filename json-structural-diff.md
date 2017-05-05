# Diffing JSON schemas to find API mismatches early

While [we don't want to generate our client code from the API documentation](https://blog.novatec-gmbh.de/the-problems-with-swagger/),
doing it by hand leaves room for error.
When (and it's always when, not if) such an error occurs, it can take until a while after delivery that the devs notice.
This usually means hotfixing, and therefore making sure that this time, the message structure was implemented correctly, with human eyes, under time pressure.

Since this is obviously a recipe for disaster, I wrote a tool to diff JSON structure for me.
I wrote it in [Haskell](https://www.haskell.org), my favorite functional programming language.

## description of algorithm, with code

## example

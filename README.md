# language-pig #

Parser and pretty printer for the Apache Pig scripting language (http://pig.apache.org/). The current version is implemented using Parsec parser combinators.

# install #

Cabal project, now on hackage, so the usual

```
cabal install language-pig
```

Or from source

```
git clone ...
cd language-pig
cabal install
```

# Useage #

Parse an expression:

```
parseString :: [Char] -> Root
```

Returns an AST (type is the root node).

Parse a file:

```
parseFile :: FilePath -> IO Root
```

Returns an AST (type = Root, which is the root node).

Pretty print the produced tree:
```
putStrLn $ prettyPrint tree
```

So to round it up, if you want to parse and pretty print the parsed AST of a Pig file (using Control.Applicative (<$>))

```
prettyPrint <$> parseFile "example.pig" >>= putStrLn
```
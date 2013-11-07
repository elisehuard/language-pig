# language-pig #

Parser and pretty printer for the Apache Pig scripting language (http://pig.apache.org/). The current version is implemented using Parsec parser combinators.

# Install #

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

# Use #

Parse an expression:

```
parseString :: [Char] -> Root
```

Returns an AST (type Root is the root node).

Parse a file:
```
parseFile :: FilePath -> IO PigFile
```
PigFile contains the Root (of AST) and the file name.

Pretty print the produced tree:
```
putStrLn $ prettyPrint tree
```

So to round it up, if you want to parse and pretty print the parsed AST of a Pig file (using Control.Applicative (<$>))

```
prettyPrint <$> parseFile "example.pig" >>= putStrLn
```

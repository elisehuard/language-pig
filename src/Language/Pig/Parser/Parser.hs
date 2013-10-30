module Language.Pig.Parser.Parser (
  parseString
  , parseFile
  , module Language.Pig.Parser.AST
) where

import System.IO
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.List (intercalate)
import Control.Applicative ((<$>), (<*>), (*>), (<*))

import Language.Pig.Parser.AST


-- Lexer

specialChar = oneOf "_"

pigLanguageDef :: LanguageDef st
pigLanguageDef = emptyDef {
            Token.commentStart = "/*"
          , Token.commentEnd = "*/"
          , Token.commentLine = "--"
          , Token.nestedComments = True
          , Token.identStart     = letter
          , Token.identLetter    = alphaNum <|> specialChar
          , Token.reservedNames = ["LOAD", "USING", "AS", -- todo case insensitivity of these keywords
                                   "FOREACH", "GENERATE", "FLATTEN",
                                   "JOIN", "BY",
                                   "GROUP",
                                   "DESCRIBE", "SHIP",
                                   "DEFINE",
                                   "STREAM", "THROUGH",
                                   "STORE", "INTO", "USING",
                                   "int", "long", "float", "double", "chararray", "bytearray", "*"]
          , Token.reservedOpNames = ["=", "+", "-", "*", "/", "%", "?", ":"]
          , Token.caseSensitive = False
        }

lexer = Token.makeTokenParser pigLanguageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
integer = Token.integer lexer
naturalOrFloat = Token.naturalOrFloat lexer
semi = Token.semi lexer
comma = Token.comma lexer
whiteSpace = Token.whiteSpace lexer
parens = Token.parens lexer
lexeme = Token.lexeme lexer

-- parser to handle double colon.
pigIdentifier = try(detailedIdentifier) <|> identifier
detailedIdentifier :: Parser String
detailedIdentifier = lexeme $
                     (intercalate "::") <$> sepBy1 identifierPart (string "::")
identifierPart = (:) <$> letter <*> many1 (alphaNum <|> specialChar)


-- Parser: top-down

parseString :: [Char] -> Root
parseString input = case parsePig input of
    Left msg -> error (show msg)
    Right p -> p

parseFile :: FilePath -> IO Root
parseFile filename = parseString <$> readFile (filename)

parsePig :: String -> Either ParseError Root
parsePig input = parse pigParser "pigParser error" input

pigParser :: Parser Root
pigParser = whiteSpace >> statements

statements :: Parser Root
statements = Seq <$> endBy statement semi

statement :: Parser Statement
statement = query <|> describe <|> define <|> store

query :: Parser Statement
query = Assignment <$> 
           (Identifier <$> identifier) <*>
           (reservedOp "=" *> opClause)

describe :: Parser Statement
describe = Describe <$> (reserved "DESCRIBE" *> pigVar)

define :: Parser Statement
define = DefineUDF <$>
           (reserved "define" *>
           pigVar) <*>
           executable <*>
           shipClause -- could be input, output, ship, cache, stderr in full pig grammar

store :: Parser Statement
store = Store <$>
        (reserved "STORE" *>
         pigVar) <*>
        (reserved "INTO" *>
         pigQuotedString Directory) <*>
        (reserved "USING" *>
         pigFunc)


opClause :: Parser OpClause
opClause = loadClause
       <|> foreachClause
       <|> innerJoinClause
       <|> groupClause
       <|> streamClause

loadClause :: Parser OpClause
loadClause = LoadClause <$>
                (reserved "LOAD" *>
                pigQuotedString Filename) <*>
                (reserved "USING" *>
                pigFunc) <*>
                (reserved "AS" *>
                pigTupleDef)

-- foreach: only the block (outer bag) version
foreachClause :: Parser OpClause
foreachClause = ForeachClause <$>
                    (reserved "FOREACH" *>
                    pigVar) <*>
                    (reserved "GENERATE" *>
                    (GenBlock <$> sepBy transform comma))

innerJoinClause :: Parser OpClause
innerJoinClause = InnerJoinClause <$>
                    (reserved "JOIN" *>
                    sepBy joinTable comma)

groupClause :: Parser OpClause
groupClause = GroupClause <$>
                (reserved "GROUP" *>
                pigVar) <*>
                (reserved "BY" *>
                (MultipleColumn <$> tuple <|> SingleColumn <$> name))

streamClause :: Parser OpClause
streamClause = StreamClause <$>
                (reserved "STREAM" *>
                pigVar) <*>
                (reserved "THROUGH" *>
                pigVar) <*>
                (reserved "AS" *>
                pigTupleDef)

joinTable :: Parser Join
joinTable = Join <$>
               pigIdentifier <*>
               (reserved "BY" *>
               pigIdentifier)

shipClause :: Parser DefineSpec
shipClause = Ship . Filename <$> 
               (reserved "SHIP" *>
                parens quotedString)

pigVar :: Parser Alias
pigVar = Identifier <$> pigIdentifier

pigQuotedString :: (String -> a) -> Parser a
pigQuotedString constructor = constructor <$> quotedString

pigFunc :: Parser Function
pigFunc = Function <$>
            identifier <*>
            parens arguments

arguments :: Parser [Argument]
arguments = sepBy argument comma

argument :: Parser Argument
argument = (StringArgument . String <$> quotedString) <|> 
           (AliasArgument <$> pigVar)

quotedString :: Parser String
quotedString = (char '\'' *> (many $ noneOf "\'")) <* char '\'' <* whiteSpace -- doesn't take into account escaped quotes

executable :: Parser Command
executable = Exec <$> (char '`' *> (many $ noneOf "`") <* char '`' <* whiteSpace)

pigTupleDef :: Parser TupleDef
pigTupleDef = TupleDef <$> parens tupleDef

tupleDef :: Parser [Field]
tupleDef = sepBy field comma

field :: Parser Field
field = Field <$>
            pigVar
            <* char ':'
            <*> pigType

pigType :: Parser SimpleType
pigType = pigSimpleType "int" Int <|>
          pigSimpleType "long" Long <|>
          pigSimpleType "float" Float <|>
          pigSimpleType "double" Double <|>
          pigSimpleType "chararray" CharArray <|>
          pigSimpleType "bytearray" ByteArray

pigSimpleType :: String -> SimpleType -> Parser SimpleType
pigSimpleType typeString constructor = reserved typeString >> return constructor
              
transform :: Parser Transform
transform = try(aliasTransform)
         <|> flattenTransform
         <|> tupleFieldGlob
         <|> expressionTransform
         <|> functionTransform
         <|> envTransform

flattenTransform :: Parser Transform
flattenTransform = Flatten <$>
                      (reserved "FLATTEN" *>
                       parens pigIdentifier) <*>
                      (reserved "AS" *>
                       tuple)

expressionTransform :: Parser Transform
expressionTransform = ExpressionTransform <$>
                       generalExpression <*>
                       (reserved "AS" *>
                        (Identifier <$> identifier))

functionTransform :: Parser Transform
functionTransform = FunctionTransform <$>
                      pigFunc <*>
                      (reserved "AS" *>
                       (Identifier <$> identifier))

aliasTransform :: Parser Transform
aliasTransform = AliasTransform <$>
                   (Identifier <$> pigIdentifier) <*>
                   (reserved "AS" *>
                   (Identifier <$> identifier))

envTransform :: Parser Transform
envTransform = EnvTransform <$>
                  pigQuotedString String <*>
                  (reserved "AS" *>
                   (Identifier <$> identifier))

-- general expression:
-- fieldExpression or literal or function or binary operation (+-*/%) or bincond (?:)
-- bincond: boolean expression (==, !=, >, <, >=, <=) (and, or, not)
generalExpression :: Parser Expression
generalExpression = parens calculation

-- conditional is ternary operator, so lookahead to try and parse it first.
calculation :: Parser Expression
calculation = try(conditional) <|> buildExpressionParser pigOperators pigTerm

pigOperators = [[Prefix (reservedOp "-" >> return (Unary Neg))]
               ,[Infix (reservedOp "*" >> return (Binary Multiply)) AssocLeft]
               ,[Infix (reservedOp "/" >> return (Binary Divide)) AssocLeft]
               ,[Infix (reservedOp "%" >> return (Binary Modulo)) AssocLeft]
               ,[Infix (reservedOp "+" >> return (Binary Add)) AssocLeft]
               ,[Infix (reservedOp "-" >> return (Binary Subtract)) AssocLeft]]

pigTerm :: Parser Expression
pigTerm = (ScalarTerm . String <$> quotedString)
      <|> (ScalarTerm <$> number)
      <|> generalExpression
      <|> (AliasTerm <$> name)

number = Number <$> naturalOrFloat -- for now - could be naturalOrFloat for inclusion

conditional :: Parser Expression
conditional = BinCond <$>
                booleanExpression <*>
                (reserved "?" *>
                 calculation) <*>
                (reserved ":" *>
                 calculation)

booleanExpression = buildExpressionParser booleanOperators booleanTerm

booleanTerm = parens booleanExpression
          <|> comparisonExpression

booleanOperators = [ [Prefix (reservedOp "not" >> return (BooleanUnary Not))]
                   , [Infix  (reservedOp "and" >> return (BooleanBinary And)) AssocLeft]
                   , [Infix  (reservedOp "or"  >> return (BooleanBinary Or)) AssocLeft]]

comparisonExpression :: Parser BooleanExpression
comparisonExpression = flippedBooleanExpression <$> pigTerm <*> relation <*> pigTerm
                      where flippedBooleanExpression expr1 op expr2 = BooleanExpression op expr1 expr2

-- bincond: boolean expression (==, !=, >, <, >=, <=) (and, or, not)
relation = (reservedOp ">" >> return Greater) <|>
           (reservedOp "<" >> return Less) <|>
           (reservedOp "<=" >> return LessEqual) <|>
           (reservedOp ">=" >> return GreaterEqual) <|>
           (reservedOp "==" >> return Equal) <|>
           (reservedOp "!=" >> return NotEqual)

tupleFieldGlob :: Parser Transform
tupleFieldGlob = reserved "*" >> return TupleFieldGlob

tuple :: Parser Tuple
tuple = Tuple <$> parens (sepBy name comma)

name :: Parser Alias
name = Identifier <$> pigIdentifier

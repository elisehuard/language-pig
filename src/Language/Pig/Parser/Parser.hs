module Language.Pig.Parser.Parser (
  parseString
  , parseFile
  , parseFileForAST
  , parseTst
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
          , Token.reservedNames = ["LOAD", "USING", "AS",
                                   "FOREACH", "GENERATE", "FLATTEN",
                                   "JOIN", "BY",
                                   "LEFT", "RIGHT", "FULL",
                                   "GROUP",
                                   "DESCRIBE", "SHIP",
                                   "DEFINE",
                                   "STREAM", "THROUGH",
                                   "STORE", "INTO", "USING",
                                   "REGISTER",
                                   "DISTINCT",
                                   "FILTER",
                                   "int", "long", "float", "double", "chararray", "bytearray", "*"]
          , Token.reservedOpNames = ["=", "+", "-", "*", "/", "%", "?", ":", "and", "or", "not"]
          , Token.caseSensitive = False
        }

lexer = Token.makeTokenParser pigLanguageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
naturalOrFloat = Token.naturalOrFloat lexer
integer = Token.integer lexer
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

functionIdentifier = try(dottedIdentifier) <|> identifier
dottedIdentifier :: Parser String
dottedIdentifier = lexeme $
                     (intercalate ".") <$> sepBy1 identifierPart (string ".")


-- Parser: top-down

parseString :: [Char] -> Root
parseString input = case parsePig input of
    Left msg -> error (show msg)
    Right p -> p

parseFile :: FilePath -> IO PigFile
parseFile filename = ((PigFile filename) . parseString) <$> readFile (filename)

parseFileForAST filename = getAST <$> parseFile filename

getAST :: PigFile -> Root
getAST (PigFile _ ast) = ast

parsePig :: String -> Either ParseError Root
parsePig input = parse pigParser "pigParser error" input

parseTst :: String -> Either ParseError Expression
parseTst input = parse conditional "test error" input

pigParser :: Parser Root
pigParser = whiteSpace >> statements

statements :: Parser Root
statements = Seq <$> endBy statement semi

statement :: Parser Statement
statement = query <|> describe <|> define <|> store <|> register

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
           aliasable <*>
           defineSpec -- could be input, output, ship, cache, stderr in full pig grammar

aliasable :: Parser Aliasable
aliasable = try (AliasCommand <$>
               executable) <|>
            (AliasFunction <$> pigFunc)

store :: Parser Statement
store = Store <$>
        (reserved "STORE" *>
         pigVar) <*>
        (reserved "INTO" *>
         pigQuotedString Directory) <*>
        (reserved "USING" *>
         pigFunc)

register :: Parser Statement
register = Register <$>
            (reserved "REGISTER" *>
             pigQuotedString Library)


opClause :: Parser OpClause
opClause = loadClause
       <|> foreachClause
       <|> joinClause
       <|> groupClause
       <|> streamClause
       <|> distinctClause
       <|> filterClause

loadClause :: Parser OpClause
loadClause = LoadClause <$>
                (reserved "LOAD" *>
                pigQuotedString Filename) <*>
                (optionMaybe (reserved "USING" *>
                pigFunc)) <*>
                (optionMaybe (reserved "AS" *>
                pigTupleDef))

-- foreach: only the block (outer bag) version
foreachClause :: Parser OpClause
foreachClause = ForeachClause <$>
                    (reserved "FOREACH" *>
                    pigVar) <*>
                    (reserved "GENERATE" *>
                    (GenBlock <$> sepBy transform comma))

joinClause :: Parser OpClause
joinClause = JoinClause <$>
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

distinctClause :: Parser OpClause
distinctClause = DistinctClause <$>
                   (reserved "DISTINCT" *>
                    pigVar)

filterClause :: Parser OpClause
filterClause = FilterClause <$>
                (reserved "FILTER" *>
                    pigVar) <*>
                (reserved "BY" *>
                    booleanExpression)

joinTable :: Parser Join
joinTable = Join <$>
               pigIdentifier <*>
               (reserved "BY" *>
                   pigIdentifier) <*>
               optionMaybe outerJoinType

outerJoinType :: Parser OuterJoinType
outerJoinType = (reserved "LEFT" >> return LeftJoin) <|>
                (reserved "RIGHT" >> return RightJoin) <|>
                (reserved "FULL" >> return FullJoin)

defineSpec :: Parser [DefineSpec]
defineSpec = many shipClause

shipClause :: Parser DefineSpec
shipClause = Ship <$> 
               (reserved "SHIP" *>
                parens shipArgs)

shipArgs :: Parser [Path]
shipArgs = sepBy1 (Filename <$> quotedString) comma

pigVar :: Parser Alias
pigVar = Identifier <$> pigIdentifier

pigQuotedString :: (String -> a) -> Parser a
pigQuotedString constructor = constructor <$> quotedString

pigFunc :: Parser Function
pigFunc = Function <$>
            functionIdentifier <*>
            parens arguments

arguments :: Parser [Expression]
arguments = sepBy argument comma

argument :: Parser Expression
argument = calculation


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
            pigVar <*>
            optionMaybe
              ( char ':' *>
                whiteSpace *>
                pigType )

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
         <|> try(positionalTypeTransform)
         <|> try(castTransform)
         <|> expressionTransform
         <|> try(functionTransform)
         <|> identityTransform
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

-- field is just used as-is from input file
identityTransform :: Parser Transform
identityTransform = IdentityTransform <$>
                      (Identifier <$> pigIdentifier)

positionalTypeTransform :: Parser Transform
positionalTypeTransform = PositionalTypeTransform <$>
                             (parens pigType) <*>
                             (char '$' *>
                                integer) <*>
                             (reserved "AS" *>
                                (Identifier <$> identifier))

castTransform :: Parser Transform
castTransform = CastTransform <$>
                  (parens pigType) <*>
                  (Identifier <$> identifier) <*>
                  (reserved "AS" *>
                     field)

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
      <|> try(FunctionTerm <$> pigFunc)
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
                   , [Prefix (reservedOp "NOT" >> return (BooleanUnary Not))]
                   , [Infix  (reservedOp "and" >> return (BooleanBinary And)) AssocLeft]
                   , [Infix  (reservedOp "AND" >> return (BooleanBinary And)) AssocLeft]
                   , [Infix  (reservedOp "or"  >> return (BooleanBinary Or)) AssocLeft]
                   , [Infix  (reservedOp "OR"  >> return (BooleanBinary Or)) AssocLeft]]

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

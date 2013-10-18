module Language.Pig.Parser.Parser (
  parseString
  , parseFile
  , module Language.Pig.Parser.AST
) where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.List (intercalate)

import Language.Pig.Parser.AST


-- Lexer

specialChar = oneOf "_" -- TODO only allow double colon

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

-- TODO: create parser to handle double colon.
pigIdentifier = try(detailedIdentifier) <|> identifier

detailedIdentifier :: Parser String
detailedIdentifier = lexeme $
                     do parts <- sepBy1 identifierPart (string "::")
                        return $ intercalate "::" parts

identifierPart = do start <- letter
                    part <- many1 (alphaNum <|> specialChar)
                    return $ (start:part)


-- Parser: top-down

pigParser :: Parser Root
pigParser = whiteSpace >> statements

statements :: Parser Root
statements = do list <- endBy statement semi
                return $ Seq list

statement :: Parser Statement
statement = query <|> describe <|> define <|> store

query :: Parser Statement
query = do var <- identifier
           reservedOp "="
           expr <- opClause
           return $ Assignment (Identifier var) expr

describe :: Parser Statement
describe = do reserved "DESCRIBE"
              variable <- pigVar
              return $ Describe variable

define :: Parser Statement
define = do reserved "define"
            alias <- pigVar
            command <- executable
            ship <- shipClause -- could be input, output, ship, cache, stderr in full pig grammar
            return $ DefineUDF alias command ship 

store :: Parser Statement
store = do reserved "STORE"
           alias <- pigVar
           reserved "INTO"
           output <- pigQuotedString Directory
           reserved "USING"
           func <- pigFunc
           return $ Store alias output func


opClause :: Parser OpClause
opClause = loadClause
       <|> foreachClause
       <|> innerJoinClause
       <|> groupClause
       <|> streamClause

loadClause :: Parser OpClause
loadClause =
  do reserved "LOAD"
     file <-  pigQuotedString Filename
     reserved "USING"
     source <- pigFunc
     reserved "AS"
     schema <- pigTupleDef
     return $ LoadClause file source schema

-- foreach: only the block (outer bag) version
foreachClause :: Parser OpClause
foreachClause =
  do reserved "FOREACH"
     alias <- pigVar
     reserved "GENERATE"
     transforms <- sepBy transform comma
     return $ ForeachClause alias (GenBlock transforms)

innerJoinClause :: Parser OpClause
innerJoinClause =
  do reserved "JOIN"
     joins <- sepBy joinTable comma
     return $ InnerJoinClause joins

groupClause :: Parser OpClause
groupClause =
  do reserved "GROUP"
     alias <- pigVar
     reserved "BY"
     columns <- liftM MultipleColumn tuple <|> liftM SingleColumn name
     return $ GroupClause alias columns

streamClause :: Parser OpClause
streamClause =
  do reserved "STREAM"
     alias <- pigVar
     reserved "THROUGH"
     udf <- pigVar
     reserved "AS"
     schema <- pigTupleDef
     return $ StreamClause alias udf schema

joinTable :: Parser Join
joinTable = do table <- pigIdentifier
               reserved "BY"
               fieldName <- pigIdentifier
               return $ Join table fieldName

shipClause :: Parser DefineSpec
shipClause = do reserved "SHIP"
                path <- parens quotedString
                return $ Ship (Filename path)

pigVar :: Parser Alias
pigVar = liftM Identifier $ pigIdentifier

pigQuotedString :: (String -> a) -> Parser a
pigQuotedString constructor = liftM constructor $ quotedString

pigFunc :: Parser Function
pigFunc = do value <- identifier
             arguments <- parens arguments
             return $ Function value arguments

arguments :: Parser [Argument]
arguments = do args <- sepBy argument comma
               return $ args

argument :: Parser Argument
argument = (liftM (StringArgument . String) quotedString) <|> 
           liftM AliasArgument pigVar

quotedString :: Parser String
quotedString = do char '\''
                  value <- many $ noneOf "\'" -- doesn't take into account escaped quotes
                  char '\''
                  whiteSpace
                  return $ value

executable :: Parser Command
executable = do char '`'
                value <- many $ noneOf "`" -- doesn't take into account escaped quotes
                char '`'
                whiteSpace
                return $ Exec value

pigTupleDef :: Parser TupleDef
pigTupleDef = liftM TupleDef $ parens tupleDef

tupleDef :: Parser [Field]
tupleDef = liftM id $ sepBy field comma

field :: Parser Field
field = do fieldName <- pigVar
           char ':'
           fieldType <- pigType
           return $ Field fieldName fieldType

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
flattenTransform = do reserved "FLATTEN"
                      argument <- parens pigIdentifier
                      reserved "AS"
                      schema <- tuple
                      return $ Flatten argument schema

expressionTransform :: Parser Transform
expressionTransform = do expr <- generalExpression
                         reserved "AS"
                         fieldName <- identifier
                         return $ ExpressionTransform expr (Identifier fieldName)

functionTransform :: Parser Transform
functionTransform = do function <- pigFunc
                       reserved "AS"
                       fieldName <- identifier
                       return $ FunctionTransform function (Identifier fieldName)

aliasTransform :: Parser Transform
aliasTransform = do name1 <- pigIdentifier
                    reserved "AS"
                    alias <- identifier
                    return $ AliasTransform (Identifier name1) (Identifier alias)

envTransform :: Parser Transform
envTransform = do name1 <- pigQuotedString String
                  reserved "AS"
                  alias <- identifier
                  return $ EnvTransform name1 (Identifier alias)

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
pigTerm = (liftM (ScalarTerm . String) $ quotedString)
      <|> (liftM ScalarTerm $ number)
      <|> generalExpression
      <|> (liftM AliasTerm $ name)

number = liftM Number $ naturalOrFloat -- for now - could be naturalOrFloat for inclusion

conditional :: Parser Expression
conditional = do cond <- booleanExpression
                 reserved "?"
                 ifTrue <- calculation
                 reserved ":"
                 ifFalse <- calculation
                 return $ BinCond cond ifTrue ifFalse

booleanExpression = buildExpressionParser booleanOperators booleanTerm

booleanTerm = parens booleanExpression
          <|> comparisonExpression

booleanOperators = [ [Prefix (reservedOp "not" >> return (BooleanUnary Not))]
                   , [Infix  (reservedOp "and" >> return (BooleanBinary And)) AssocLeft]
                   , [Infix  (reservedOp "or"  >> return (BooleanBinary Or)) AssocLeft]]

comparisonExpression :: Parser Expression
comparisonExpression = do term1 <- pigTerm
                          operator <- relation
                          term2 <- pigTerm
                          return $ Binary operator term1 term2

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
tuple = liftM Tuple $ parens (sepBy name comma)

name :: Parser Alias
name = liftM Identifier $ pigIdentifier

-- top-level parse functions

parsePig :: String -> Either ParseError Root
parsePig input = parse pigParser "pigParser error" input

parseString :: [Char] -> Root
parseString input = case parsePig input of
    Left msg -> error (show msg)
    Right p -> p

parseFile :: FilePath -> IO String
parseFile filename =
  do
     x <- readFile (filename)
     return $ show $ parseString x

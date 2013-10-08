module Language.Pig.Parser.Parser (
  parseString
  , parseFile
  , PigNode(..)
) where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

--import Language.Pig.Parser.AST

data PigNode = PigStmt PigNode
             | PigQuery PigNode PigNode
             | PigDescribe PigNode
             | PigIdentifier String
             | PigOpClause PigNode
             | PigLoadClause PigNode PigNode PigNode
             | PigForeachClause PigNode PigNode
             | PigInnerJoinClause [PigNode]
             | PigGroupClause PigNode PigNode
             | PigDefineUDF PigNode PigNode PigNode
             | PigStreamClause PigNode PigNode PigNode
             | PigStore PigNode PigNode PigNode
             | PigShip PigNode
             | PigFilename String
             | PigDirectory String
             | PigExec String
             | PigPath String
             | PigFunc String PigNode
             | PigArguments [PigNode]
             | PigSchema [PigNode]
             | PigField PigNode PigNode
             | PigFieldName String
             | PigFieldType PigNode
             | PigTransforms [PigNode]
             | PigFlatten String PigNode -- foreach flatten transform
             | PigTupleFieldGlob
             | PigTuple [PigNode]
             | PigExpressionTransform PigNode PigNode -- foreach calculates expression
             | PigExpression PigNode
             | PigJoin String String
             | PigBinary PigNode PigNode PigNode
             | PigBooleanBinary PigNode PigNode PigNode
             | PigBinCond PigNode PigNode PigNode
             | PigString String
             | PigInt
             | PigLong
             | PigFloat
             | PigDouble
             | PigCharArray
             | PigByteArray
             | PigAdd
             | PigSubtract
             | PigMultiply
             | PigDivide
             | PigModulo
             | PigNeg PigNode
             | PigNumber (Either Integer Double)
             | PigStringLiteral String
             | PigAnd
             | PigOr
             | PigNot PigNode
             | PigEqual
             | PigNotEqual
             | PigGreater
             | PigLess
             | PigGreaterEqual
             | PigLessEqual
             deriving (Show, Eq) -- Read, Data, Typeable ?

specialChar = oneOf "_" -- TODO only allow double colon

pigLanguageDef :: LanguageDef st
pigLanguageDef = emptyDef {
            Token.commentStart = "/*"
          , Token.commentEnd = "*/"
          , Token.commentLine = "--"
          , Token.nestedComments = True
          , Token.identStart     = letter
          , Token.identLetter    = alphaNum <|> specialChar -- todo allow double colon in identifier: custom parser
          , Token.reservedNames = ["LOAD", "USING", "AS", -- todo case insensitivity of these keywords
                                   "FOREACH", "GENERATE", "FLATTEN",
                                   "JOIN", "BY",
                                   "GROUP",
                                   "DESCRIBE", "SHIP",
                                   "define",
                                   "STREAM", "THROUGH",
                                   "STORE", "INTO", "USING",
                                   "int", "long", "float", "double", "chararray", "bytearray", "*"]
          , Token.reservedOpNames = ["=", "+", "-", "*", "/", "%", "?", ":"]
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

-- TODO: create parser to handle double colon.
pigIdentifier = identifier

pigParser :: Parser PigNode
pigParser = whiteSpace >> statements -- leading whitespace

statements :: Parser PigNode
statements = liftM head $ endBy statement semi -- TODO handle multiple statements

statement :: Parser PigNode
statement = query <|> describe <|> define <|> store

query :: Parser PigNode
query = do var <- pigVar
           reservedOp "="
           expr <- opClause
           return $ PigQuery var expr

describe :: Parser PigNode
describe = do reserved "DESCRIBE"
              variable <- pigVar
              return $ PigDescribe variable

define :: Parser PigNode
define = do reserved "define"
            alias <- pigVar
            command <- executable
            ship <- shipClause -- could be input, output, ship, cache, stderr in full pig grammar
            return $ PigDefineUDF alias command ship 

store :: Parser PigNode
store = do reserved "STORE"
           alias <- pigVar
           reserved "INTO"
           output <- pigQuotedString PigDirectory
           reserved "USING"
           func <- pigFunc
           return $ PigStore alias output func


opClause :: Parser PigNode
opClause = loadClause
       <|> foreachClause
       <|> innerJoinClause
       <|> groupClause
       <|> streamClause

loadClause :: Parser PigNode
loadClause =
  do reserved "LOAD"
     file <-  pigQuotedString PigFilename
     reserved "USING"
     source <- pigFunc
     reserved "AS"
     schema <- pigTupleDef
     return $ PigLoadClause file source schema

-- foreach: only the block (outer bag) version
foreachClause :: Parser PigNode
foreachClause =
  do reserved "FOREACH"
     alias <- pigVar
     reserved "GENERATE"
     transforms <- sepBy transform comma
     return $ PigForeachClause alias (PigTransforms transforms)

innerJoinClause :: Parser PigNode
innerJoinClause =
  do reserved "JOIN"
     joins <- sepBy joinTable comma
     return $ PigInnerJoinClause joins

groupClause :: Parser PigNode
groupClause =
  do reserved "GROUP"
     alias <- pigVar
     reserved "BY"
     columns <- tuple <|> name
     return $ PigGroupClause alias columns

streamClause :: Parser PigNode
streamClause =
  do reserved "STREAM"
     alias <- pigVar
     reserved "THROUGH"
     udf <- pigVar
     reserved "AS"
     schema <- pigTupleDef
     return $ PigStreamClause alias udf schema

joinTable :: Parser PigNode
joinTable = do table <- pigIdentifier
               reserved "BY"
               fieldName <- pigIdentifier
               return $ PigJoin table fieldName

shipClause :: Parser PigNode
shipClause = do reserved "SHIP"
                path <- parens quotedString
                return $ PigShip (PigPath path)

pigVar :: Parser PigNode
pigVar = liftM PigIdentifier $ pigIdentifier

pigQuotedString :: (String -> PigNode) -> Parser PigNode
pigQuotedString constructor = liftM constructor $ quotedString

pigFunc :: Parser PigNode
pigFunc = do value <- identifier
             arguments <- parens arguments
             return $ PigFunc value arguments

arguments :: Parser PigNode
arguments = liftM PigArguments $ sepBy argument comma

argument :: Parser PigNode
argument = (liftM PigString quotedString) <|> 
           (liftM PigFieldName pigIdentifier)

quotedString :: Parser String
quotedString = do char '\''
                  value <- many $ noneOf "\'" -- doesn't take into account escaped quotes
                  char '\''
                  whiteSpace
                  return $ value

executable :: Parser PigNode
executable = do char '`'
                value <- many $ noneOf "`" -- doesn't take into account escaped quotes
                char '`'
                whiteSpace
                return $ PigExec value

pigTupleDef :: Parser PigNode
pigTupleDef = liftM PigSchema $ parens tupleDef

tupleDef :: Parser [PigNode]
tupleDef = liftM id $ sepBy field comma

field :: Parser PigNode
field = do fieldName <- pigIdentifier
           char ':'
           fieldType <- pigType
           return $ PigField (PigFieldName fieldName) (PigFieldType fieldType)

pigType :: Parser PigNode
pigType = pigSimpleType "int" PigInt <|>
          pigSimpleType "long" PigLong <|>
          pigSimpleType "float" PigFloat <|>
          pigSimpleType "double" PigDouble <|>
          pigSimpleType "chararray" PigCharArray <|>
          pigSimpleType "bytearray" PigByteArray

pigSimpleType :: String -> PigNode -> Parser PigNode
pigSimpleType typeString constructor = reserved typeString >> return constructor
              
transform :: Parser PigNode
transform = flattenTransform <|> tupleFieldGlob <|> expressionTransform

flattenTransform :: Parser PigNode
flattenTransform = do reserved "FLATTEN"
                      argument <- parens pigIdentifier
                      reserved "AS"
                      schema <- tuple
                      return $ PigFlatten argument schema

expressionTransform :: Parser PigNode
expressionTransform = do expr <- expression
                         reserved "AS"
                         fieldName <- pigIdentifier
                         return $ PigExpressionTransform expr (PigFieldName fieldName)

expression :: Parser PigNode
expression = tupleFieldGlob <|> pigFunc <|> name <|> (pigQuotedString PigString) <|> generalExpression

-- general expression:
-- fieldExpression or literal or function or binary operation (+-*/%) or bincond (?:)
-- bincond: boolean expression (==, !=, >, <, >=, <=) (and, or, not)
generalExpression :: Parser PigNode
generalExpression = parens calculation

-- conditional is ternary operator, so lookahead to try and parse it first.
calculation :: Parser PigNode
calculation = try(conditional) <|> buildExpressionParser pigOperators pigTerm

pigOperators = [[Prefix (reservedOp "-" >> return (PigNeg))]
               ,[Infix (reservedOp "*" >> return (PigBinary PigMultiply)) AssocLeft]
               ,[Infix (reservedOp "/" >> return (PigBinary PigDivide)) AssocLeft]
               ,[Infix (reservedOp "%" >> return (PigBinary PigModulo)) AssocLeft]
               ,[Infix (reservedOp "+" >> return (PigBinary PigAdd)) AssocLeft]
               ,[Infix (reservedOp "-" >> return (PigBinary PigSubtract)) AssocLeft]]

pigTerm = (liftM PigStringLiteral $ quotedString) <|> number <|> name <|> generalExpression

number = liftM PigNumber $ naturalOrFloat -- for now - could be naturalOrFloat for inclusion

conditional = do cond <- booleanExpression
                 reserved "?"
                 ifTrue <- calculation
                 reserved ":"
                 ifFalse <- calculation
                 return $ PigBinCond cond ifTrue ifFalse

booleanExpression = buildExpressionParser booleanOperators booleanTerm

booleanTerm = parens booleanExpression
          <|> comparisonExpression

booleanOperators = [ [Prefix (reservedOp "not" >> return (PigNot))]
                   , [Infix  (reservedOp "and" >> return (PigBooleanBinary PigAnd)) AssocLeft]
                   , [Infix  (reservedOp "or"  >> return (PigBooleanBinary PigOr)) AssocLeft]]

comparisonExpression = do term1 <- pigTerm
                          operator <- relation
                          term2 <- pigTerm
                          return $ PigBinary operator term1 term2

-- bincond: boolean expression (==, !=, >, <, >=, <=) (and, or, not)
relation = (reservedOp ">" >> return PigGreater) <|>
           (reservedOp "<" >> return PigLess) <|>
           (reservedOp "<=" >> return PigLessEqual) <|>
           (reservedOp ">=" >> return PigGreaterEqual) <|>
           (reservedOp "==" >> return PigEqual) <|>
           (reservedOp "!=" >> return PigNotEqual)

tupleFieldGlob :: Parser PigNode
tupleFieldGlob = reserved "*" >> return PigTupleFieldGlob

tuple :: Parser PigNode
tuple = liftM PigTuple $ parens (sepBy name comma)

name :: Parser PigNode
name = liftM PigFieldName $ pigIdentifier

-- top-level parse functions

parseString :: String -> Either ParseError PigNode
parseString input = parse pigParser "pigParser error" input

readPig :: [Char] -> PigNode
readPig input = case parseString input of
    Left msg -> error (show msg)
    Right p -> p

parseFile :: FilePath -> IO String
parseFile filename =
  do
     x <- readFile (filename)
     return $ show $ readPig x

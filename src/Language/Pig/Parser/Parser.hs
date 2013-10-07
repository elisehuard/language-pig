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
             | PigIdentifier String
             | PigOpClause PigNode
             | PigLoadClause PigNode PigNode PigNode
             | PigForeachClause String PigNode
             | PigFilename String
             | PigFunc String PigNode
             | PigArguments [String]
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
             | PigBinary PigNode PigNode PigNode
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
             | PigNeg
             | PigNumber (Either Integer Double)
             | PigStringLiteral String
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
          , Token.reservedNames = ["LOAD", "USING", "AS", 
                                   "FOREACH", "GENERATE", "FLATTEN",
                                   "int", "long", "float", "double", "chararray", "bytearray", "*"]
          , Token.reservedOpNames = ["=", "+", "-", "*", "/", "%"]
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

pigParser :: Parser PigNode
pigParser = whiteSpace >> statements -- leading whitespace

statements :: Parser PigNode
statements = liftM head $ endBy statement semi -- TODO handle multiple statements

statement :: Parser PigNode
statement =
        do var <- pigIdentifier
           reservedOp "="
           expr <- opClause
           return $ PigQuery var expr

opClause :: Parser PigNode
opClause = loadClause <|> foreachClause

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
     alias <- identifier
     reserved "GENERATE"
     transforms <- sepBy transform comma
     return $ PigForeachClause alias (PigTransforms transforms)

pigIdentifier :: Parser PigNode
pigIdentifier = liftM PigIdentifier $ identifier

pigQuotedString :: (String -> PigNode) -> Parser PigNode
pigQuotedString constructor = liftM constructor $ quotedString

pigFunc :: Parser PigNode
pigFunc = do value <- identifier
             arguments <- parens arguments
             return $ PigFunc value arguments

arguments :: Parser PigNode
arguments = liftM PigArguments $ sepBy quotedString comma

quotedString :: Parser String
quotedString = do char '\''
                  value <- many $ noneOf "\'" -- doesn't take into account escaped quotes
                  char '\''
                  whiteSpace
                  return $ value

pigTupleDef :: Parser PigNode
pigTupleDef = liftM PigSchema $ parens tupleDef

tupleDef :: Parser [PigNode]
tupleDef = liftM id $ sepBy field comma

field :: Parser PigNode
field = do fieldName <- identifier
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
pigSimpleType typeString constructor = do reserved typeString
                                          return $ constructor
              
transform :: Parser PigNode
transform = flattenTransform <|> tupleFieldGlob <|> expressionTransform

flattenTransform :: Parser PigNode
flattenTransform = do reserved "FLATTEN"
                      argument <- parens identifier
                      reserved "AS"
                      schema <- parens tuple
                      return $ PigFlatten argument schema

expressionTransform :: Parser PigNode
expressionTransform = do expr <- expression
                         reserved "AS"
                         fieldName <- identifier
                         return $ PigExpressionTransform expr (PigFieldName fieldName)

expression :: Parser PigNode
expression = tupleFieldGlob <|> generalExpression <|> pigFunc

fieldExpression :: Parser PigNode
fieldExpression = name

-- general expression:
-- fieldExpression or literal or function or binary operation (+-*/%) or bincond (?:)
-- bincond: boolean expression (==, !=, >, <, >=, <=) (and, or, not)
generalExpression :: Parser PigNode
generalExpression = parens calculation

calculation :: Parser PigNode
calculation = buildExpressionParser pigOperators pigTerm

pigOperators = [-- [Prefix (reservedOp "-" >> return (PigNeg))],
               [Infix (reservedOp "*" >> return (PigBinary PigMultiply)) AssocLeft]
               ,[Infix (reservedOp "/" >> return (PigBinary PigDivide)) AssocLeft]
               ,[Infix (reservedOp "%" >> return (PigBinary PigModulo)) AssocLeft]
               ,[Infix (reservedOp "+" >> return (PigBinary PigAdd)) AssocLeft]
               ,[Infix (reservedOp "-" >> return (PigBinary PigSubtract)) AssocLeft]]

pigTerm = (liftM PigStringLiteral $ quotedString) <|> number <|> name <|> generalExpression

number = liftM PigNumber $ naturalOrFloat -- for now - could be naturalOrFloat for inclusion

tupleFieldGlob :: Parser PigNode
tupleFieldGlob = do reserved "*"
                    return $ PigTupleFieldGlob

tuple :: Parser PigNode
tuple = do fieldNames <- sepBy name comma
           return $ PigTuple fieldNames

name :: Parser PigNode
name = liftM PigFieldName $ identifier

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

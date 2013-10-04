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
             | PigFilename String
             | PigFunc String PigNode
             | PigArguments [String]
             | PigSchema String
             deriving (Show) -- Eq, Read, Data, Typeable ?

specialChar = oneOf "_:" -- TODO only allow double colon

pigLanguageDef :: LanguageDef st
pigLanguageDef = emptyDef {
            Token.commentStart = "/*"
          , Token.commentEnd = "*/"
          , Token.commentLine = "--"
          , Token.nestedComments = True
          , Token.identStart     = letter
          , Token.identLetter    = alphaNum <|> specialChar
          , Token.reservedNames = ["LOAD","USING","AS"]
          , Token.reservedOpNames = ["="]
        }

lexer = Token.makeTokenParser pigLanguageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
integer = Token.integer lexer
semi = Token.semi lexer
comma = Token.comma lexer
whiteSpace = Token.whiteSpace lexer
parens = Token.parens lexer

pigParser :: Parser PigNode
pigParser = whiteSpace >> statements -- leading whitespace

statements :: Parser PigNode
statements = do list <- (endBy statement semi)
                return $ head list

statement :: Parser PigNode
statement =
        do var <- pigIdentifier
           reservedOp "="
           expr <- opClause
           return $ PigQuery var expr

pigIdentifier :: Parser PigNode
pigIdentifier = do value <- identifier
                   return $ PigIdentifier value

-- TODO: quoted string, func, schema grammar
pigQuotedString :: (String -> PigNode) -> Parser PigNode
pigQuotedString constructor = do value <- identifier
                                 return $ constructor value

pigFunc :: (String -> PigNode -> PigNode) -> Parser PigNode
pigFunc constructor = do value <- identifier
                         arguments <- parens arguments
                         return $ constructor value arguments

arguments :: Parser PigNode
arguments = do list <- (sepBy quotedString comma)
               return $ PigArguments list

quotedString :: Parser String
quotedString = do char '\''
                  value <- many $ noneOf "\'" -- doesn't take into account escaped quotes
                  char '\''
                  return $ value

pigTupleDef :: (String -> PigNode) -> Parser PigNode
pigTupleDef constructor = do value <- identifier
                             return $ constructor value

opClause :: Parser PigNode
opClause = loadClause

loadClause :: Parser PigNode
loadClause =
  do reserved "LOAD"
     file <-  pigQuotedString PigFilename
     reserved "USING"
     source <- pigFunc PigFunc
     reserved "AS"
     schema <- pigTupleDef PigSchema
     return $ PigLoadClause file source schema

parseString :: String -> Either ParseError PigNode
parseString input = parse pigParser "pigParser error" input

readPig :: [Char] -> PigNode
readPig input = case parseString input of
    Left msg -> error (show msg)
    Right p -> p

-- | Parse the given file.
parseFile :: FilePath -> IO String
parseFile filename =
  do
     x <- readFile (filename)
     return $ show $ readPig x

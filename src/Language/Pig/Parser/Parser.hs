module Language.Pig.Parser.Parser (
  parseExpr
  , parseFile
) where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- source:
-- http://wiki.apache.org/pig/PigLexer
-- http://wiki.apache.org/pig/PigParser

data PigExpr = PigAssign String PigStmt
             deriving (Show)

data PigVar = PigVar String
             deriving (Show)

data PigStmt = PigLoadStmt String String String
             deriving (Show)


pigLanguageDef :: LanguageDef st
pigLanguageDef = emptyDef {
            Token.commentStart = "/*"
          , Token.commentEnd = "*/"
          , Token.commentLine = "--"
          , Token.nestedComments = True
          , Token.identStart     = letter
          , Token.identLetter    = alphaNum <|> oneOf "_'():$"
          , Token.reservedNames = ["LOAD","USING","AS"]
          , Token.reservedOpNames = ["="]
        }

lexer = Token.makeTokenParser pigLanguageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

pigParser :: Parser PigExpr
pigParser = whiteSpace >> statement

statement :: Parser PigExpr
statement = do list <- (endBy expression semi)
               return $ head list

expression :: Parser PigExpr
expression =
        do var <- identifier
           reservedOp "="
           expr <- statement'
           return $ PigAssign var expr

statement' :: Parser PigStmt
statement' =  loadStmt

argument :: Parser String
argument = identifier

loadStmt :: Parser PigStmt
loadStmt =
  do reserved "LOAD"
     file <-  argument
     reserved "USING"
     source <- argument
     reserved "AS"
     schema <- argument
     return $ PigLoadStmt file source schema

-- statement' :: Parser Stmt
-- statement' = loadStmt

-- parseString :: String -> Stmt

--parseExpr :: String -- ^ the input stream (Pig source code)
--      -> Either String [[String]] -- Either String AST.PigNode ^ Error or AST (type to be defined)

--parseExpr input = case parse lexParser "pig" input of
--  Left err -> Left (show err)
--  Right val -> Right val

parseExpr :: String -> Either ParseError PigExpr
parseExpr input = parse pigParser "pigFile" "x = LOAD x USING y AS z;"

readPig :: [Char] -> PigExpr
readPig input = case parseExpr input of
    Left msg -> error (show msg)
    Right p -> p

-- | Parse the given file.
parseFile :: FilePath -> IO String
parseFile filename =
  do
     x <- readFile (filename)
     return $ show $ readPig x

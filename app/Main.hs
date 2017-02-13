{-# LANGUAGE RecordWildCards #-}
module Main where

import Lib
import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import System.IO


type Identifier = String

data TokenType = TokenTypePlus | 
                 TokenTypeMinus | 
                 TokenTypeDiv | 
                 TokenTypeMultiply | 
                 TokenTypeIdent Identifier | 
                 TokenTypeSymbol String | 
                 TokenTypeRawNumber String

instance Show TokenType where
  show TokenTypePlus = "+"
  show TokenTypeMinus = "-"
  show TokenTypeDiv = "/"
  show TokenTypeMultiply = "*"
  show (TokenTypeIdent id)= "id-" ++ id
  show (TokenTypeRawNumber num) = "num-" ++ num
  show (TokenTypeSymbol sym) = "sym-" ++ sym

data Trivia = Trivia {
  sourcePos :: SourcePos
} deriving(Show)

data Token = Token {
  tokenType :: TokenType,
  tokenTrivia :: Trivia
}

instance (Show Token) where
  show (Token{..}) = show tokenType ++ "(" ++  show line ++ ":" ++ show col ++ ")" where
                     line = sourceLine (sourcePos tokenTrivia)
                     col = sourceColumn (sourcePos tokenTrivia)


data ExprNode = ExprNodeBinop(ExprNode, Token, ExprNode) |
               ExprNodeFnApplication(ExprNode, ExprNode) deriving(Show)


data Binding = Binding {
  bindingName :: Identifier,
  bindingArgNames :: [Identifier],
  bindingExpr :: ExprNode
} deriving(Show)

identifierParser :: GenParser Char st Identifier
identifierParser = do
    c <- letter
    later <- many (alphaNum <|> oneOf ['_', '-', '?'])
    return $ c:later

numberParser :: GenParser Char st TokenType
numberParser = do
  number_str <- many1 digit
  return $ TokenTypeRawNumber number_str

keywordParser :: GenParser Char st TokenType
keywordParser = do
    name <- identifierParser
    return $ TokenTypeIdent name

symbolParser :: GenParser Char st TokenType
symbolParser = do
    symbol <- many1 (oneOf "!@#$%^&*()-=+/\\?<>.|")
    return $ TokenTypeSymbol symbol

tokenParser :: GenParser Char st Token
tokenParser = do
  tokenType <- numberParser <|> keywordParser <|> symbolParser
  sourcePos <- getPosition
  let trivia = Trivia sourcePos
  return $ Token tokenType trivia

{-
-- [a-zA-Z](a-z | A-Z | 0-9 | _ | - | ?)*
identifierParser :: GenParser Char st Identifier
identifierParser = do
    c <- letter
    later <- many (alphaNum <|> oneOf ['_', '-', '?'])
    return $ c:later

simplexParser :: GenParser Char st [Binding]
simplexParser = 
    do result <- many (bindingParser <* spaces)
       spaces
       eof
       return result

tokenParser :: GenParser Char st Token
tokenParser = tokenIdentParser <|> tokenSymbolParser

exprParser :: GenParser Char st ExprNode
exprParser = do
    

bindingParser :: GenParser Char st Binding
bindingParser = do
  name <- identifierParser
  skipMany1 spaces
  args <- many (identifierParser <* spaces)
  spaces
  expr <- exprParser

  return $ Binding name args expr


type Program = [Binding]

parseSimplex :: String -> Either ParseError [Program]
parseSimplex input = parse csvFile "(unknown)" input
-}

main :: IO ()
main = do
  putStr "\n>"
  hFlush stdout
  line <- getLine
  if line == "exit"
  then return ()
  else do
      let parsed = parse (many (tokenParser <* spaces)) "repl" line 
      print parsed
      main

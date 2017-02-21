{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lib
import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import System.IO
import System.Console.Haskeline
import Control.Monad.Trans.Class
import Control.Lens

type Identifier = String

data TokenType = TokenTypePlus | 
                 TokenTypeMinus | 
                 TokenTypeDiv | 
                 TokenTypeMultiply | 
                 TokenTypeIdent Identifier | 
                 TokenTypeSymbol String | 
                 TokenTypeLet |
                 TokenTypeCase |
                 TokenTypeDefine |
                 TokenTypeLambda |
                 TokenTypeThinArrow |
                 TokenTypeFatArrow |
                 TokenTypeSemicolon |
                 TokenTypeEquals | 
                 TokenTypeRawNumber String

makePrisms ''TokenType

instance Show TokenType where
  show TokenTypePlus = "+"
  show TokenTypeMinus = "-"
  show TokenTypeDiv = "/"
  show TokenTypeMultiply = "*"
  show (TokenTypeIdent id)= "id-" ++ id
  show (TokenTypeRawNumber num) = "num-" ++ num
  show (TokenTypeSymbol sym) = "sym-" ++ sym
  show TokenTypeLet = "let"
  show TokenTypeCase = "case"
  show TokenTypeDefine = "define"
  show TokenTypeLambda = "\\"
  show TokenTypeThinArrow = "->"
  show TokenTypeFatArrow = "=>"
  show TokenTypeEquals = "="
  show TokenTypeSemicolon = ";"

  
data Trivia = Trivia {
  _triviaSourcePos :: SourcePos
} deriving(Show)

makeLenses ''Trivia

data Token = Token {
  _tokenType :: TokenType,
  _tokenTrivia :: Trivia
}

makeLenses ''Token

instance (Show Token) where
  show (Token{..}) = show _tokenType ++ "(" ++  show line ++ ":" ++ show col ++ ")" where
                     line = _tokenTrivia ^. triviaSourcePos & sourceLine
                     col =  _tokenTrivia ^. triviaSourcePos & sourceColumn


data ExprNode = ExprNodeBinop(ExprNode, Token, ExprNode) |
               ExprNodeFnApplication(ExprNode, ExprNode) |
               ExprNodeIdent Identifier deriving(Show)


makePrisms ''ExprNode


data Binding = Binding {
  _bindingName :: Identifier,
  _bindingArgNames :: [Identifier],
  _bindingExpr :: ExprNode
} deriving(Show)

makeLenses ''Binding


identfierTokenizer :: GenParser Char st Identifier
identfierTokenizer = do
    c <- letter
    later <- many (alphaNum <|> oneOf ['_', '-', '?'])
    return $ c:later

numberTokenizer :: GenParser Char st TokenType
numberTokenizer = do
  number_str <- many1 digit
  return $ TokenTypeRawNumber number_str

keywordTokenizer :: GenParser Char st TokenType
keywordTokenizer = do
    name <- identfierTokenizer
    return $ TokenTypeIdent name

makeSymbolTokenizer :: String -> TokenType -> GenParser Char st TokenType
makeSymbolTokenizer str tokenType = fmap (const tokenType) (string str) 

symbolTokenizer :: GenParser Char st TokenType
symbolTokenizer = do
    let lambda = makeSymbolTokenizer "\\" TokenTypeLambda
    let thinArrow = makeSymbolTokenizer "->" TokenTypeThinArrow
    let fatArrow = makeSymbolTokenizer "=>" TokenTypeFatArrow
    let semicolon = makeSymbolTokenizer ";" TokenTypeSemicolon
    let equals = makeSymbolTokenizer ";" TokenTypeEquals

    lambda <|> thinArrow <|> fatArrow <|> semicolon <|> equals

tokenizer :: GenParser Char st Token
tokenizer = do
  tokenType <- numberTokenizer <|> keywordTokenizer <|> symbolTokenizer
  sourcePos <- getPosition
  let trivia = Trivia sourcePos
  return $ Token tokenType trivia

tokenize :: [Char] -> Either ParseError [Token]
tokenize input = parse (many (tokenizer <* spaces)) "tokenizer" input



simplexParser :: GenParser Token st [Binding]
simplexParser = 
    do result <- many (bindingParser)
       return result

    
istoken :: (TokenType -> Maybe a) -> GenParser Token st a
istoken pred = tokenPrim show nextpos acceptor where
  nextpos _ token _ = token ^. tokenTrivia . triviaSourcePos
  acceptor token =  token ^. tokenType & pred
                
identifier :: GenParser Token st String
identifier = istoken (\case
                      TokenTypeIdent(ident) -> Just ident
                      _ -> Nothing)


exprParser :: GenParser Token st ExprNode
exprParser = do
  return $ ExprNodeIdent "a"

bindingParser :: GenParser Token st Binding
bindingParser = do
  istoken (^? _TokenTypeDefine)
  name <- identifier
  args <- many identifier
  istoken (^? _TokenTypeEquals)
  expr <- exprParser
  return $ Binding name args expr


type Program = [Binding]

programParser :: GenParser Token st Program
programParser = many bindingParser

parseSimplex :: [Token] -> Either ParseError Program
parseSimplex tokens = parse programParser "(unknown)" tokens


repl :: InputT IO ()
repl = do 
    line <- getInputLine ">"
    case line of
        Nothing -> return ()
        Just ("exit") -> return ()
        Just ("quit") -> return ()
        Just(line) -> do
                        let tokens = tokenize line
                        lift . print $ tokens
                        -- let parsed = parseSimplex tokens 
                        -- print parsed
                        repl

main :: IO ()
main = runInputT defaultSettings repl

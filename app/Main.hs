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

newtype Identifier = Identifier { _getIdentifier :: String }
makeLenses ''Identifier

instance Show Identifier where
  show ident = ident ^. getIdentifier & show

newtype RawNumber = RawNumber { _getRawNumber :: String }
makeLenses ''RawNumber

instance Show RawNumber where
  show rawnum = rawnum ^. getRawNumber & show

newtype Symbol = Symbol { _getSymbol :: String }
makeLenses ''Symbol

instance Show Symbol where
  show sym = sym ^. getSymbol & show





data TokenType = TokenTypePlus | 
                 TokenTypeMinus | 
                 TokenTypeDiv | 
                 TokenTypeMultiply | 
                 TokenTypeIdentifier Identifier | 
                 TokenTypeSymbol Symbol | 
                 TokenTypeLet |
                 TokenTypeCase |
                 TokenTypeDefine |
                 TokenTypeLambda |
                 TokenTypeThinArrow |
                 TokenTypeFatArrow |
                 TokenTypeSemicolon |
                 TokenTypeEquals | 
                 TokenTypeRawNumber RawNumber

makePrisms ''TokenType

instance Show TokenType where
  show TokenTypePlus = "+"
  show TokenTypeMinus = "-"
  show TokenTypeDiv = "/"
  show TokenTypeMultiply = "*"
  show (TokenTypeIdentifier ident)= "id-" ++ show ident
  show (TokenTypeRawNumber num) = "num-" ++ show num
  show (TokenTypeSymbol sym) = "sym-" ++ show sym
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
               ExprNodeIdent Identifier |
               ExprNodeRawNumber RawNumber
               deriving (Show)


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
    return $ Identifier (c:later)

numberTokenizer :: GenParser Char st TokenType
numberTokenizer = do
  number_str <- many1 digit
  return $ TokenTypeRawNumber (RawNumber number_str)

alphanumericTokenizer :: GenParser Char st TokenType
alphanumericTokenizer = do
    ident <- identfierTokenizer
    return $ case ident ^. getIdentifier of
                "define" -> TokenTypeDefine
                _ -> TokenTypeIdentifier ident


makeSymbolTokenizer :: String -> TokenType -> GenParser Char st TokenType
makeSymbolTokenizer str tokenType = fmap (const tokenType) (string str) 

symbolTokenizer :: GenParser Char st TokenType
symbolTokenizer = do
    let lambda = makeSymbolTokenizer "\\" TokenTypeLambda
    let thinArrow = makeSymbolTokenizer "->" TokenTypeThinArrow
    let fatArrow = makeSymbolTokenizer "=>" TokenTypeFatArrow
    let semicolon = makeSymbolTokenizer ";" TokenTypeSemicolon
    let equals = makeSymbolTokenizer "=" TokenTypeEquals

    equals <|> lambda <|> thinArrow <|> fatArrow <|> semicolon

tokenizer :: GenParser Char st Token
tokenizer = do
  tokenType <- numberTokenizer <|> alphanumericTokenizer <|> symbolTokenizer
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
                
identifierp :: GenParser Token st Identifier
identifierp = istoken (^? _TokenTypeIdentifier)

rawnumberp :: GenParser Token st RawNumber
rawnumberp = istoken (^? _TokenTypeRawNumber)

atomicExpr :: GenParser Token st ExprNode
atomicExpr = (ExprNodeIdent <$> identifierp) <|> 
             (ExprNodeRawNumber <$> rawnumberp)

exprp :: GenParser Token st ExprNode
exprp = atomicExpr

-- define <name>[args]* = <expr>
bindingParser :: GenParser Token st Binding
bindingParser = do
  istoken (^? _TokenTypeDefine)
  name <- identifierp
  args <- many identifierp
  istoken (^? _TokenTypeEquals)
  expr <- exprp
  return $ Binding name args expr


type Program = [Binding]

programParser :: GenParser Token st Program
programParser = (\x -> [x]) <$> bindingParser

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
                        let parsed = tokens >>= parseSimplex 
                        lift . print $ parsed
                        repl

main :: IO ()
main = runInputT defaultSettings repl

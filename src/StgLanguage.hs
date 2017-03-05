{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module StgLanguage where

import Control.Monad.Trans.Class
import Control.Lens
import Text.ParserCombinators.Parsec


newtype Identifier = Identifier { _getIdentifier :: String } deriving(Ord, Eq)
makeLenses ''Identifier

instance Show Identifier where
  show ident = ident ^. getIdentifier & show

newtype RawNumber = RawNumber { _getRawNumber :: String } 
makeLenses ''RawNumber

instance Show RawNumber where
  show rawnum = rawnum ^. getRawNumber & show


data TokenType = TokenTypePlus | 
                 TokenTypeMinus | 
                 TokenTypeDiv | 
                 TokenTypeMultiply | 
                 TokenTypeIdentifier Identifier | 
                 TokenTypeLet |
                 TokenTypeCase |
                 TokenTypeDefine |
                 TokenTypeUpdate Bool | -- true: should update, false: no update
                 TokenTypeThinArrow |
                 TokenTypeFatArrow |
                 TokenTypeSemicolon |
                 TokenTypeOpenBrace |
                 TokenTypeCloseBrace |
                 TokenTypeComma |
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
  show TokenTypeLet = "let"
  show TokenTypeCase = "case"
  show TokenTypeDefine = "define"
  show (TokenTypeUpdate b) = "\\" ++ (if b then "u" else "n")
  show TokenTypeThinArrow = "->"
  show TokenTypeFatArrow = "=>"
  show TokenTypeEquals = "="
  show TokenTypeSemicolon = ";"
  show TokenTypeOpenBrace = "{"
  show TokenTypeCloseBrace = "}"
  show TokenTypeComma = "}"


  
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
                     line = _tokenTrivia ^. triviaSourcePos . to sourceLine
                     col =  _tokenTrivia ^. triviaSourcePos . to sourceColumn


data Atom = AtomNumber RawNumber | AtomIdentifier Identifier deriving(Show)
makeLenses ''Atom

data ExprNode = ExprNodeBinop ExprNode Token ExprNode |
               ExprNodeFnApplication Identifier [Atom]
               deriving (Show)

makePrisms ''ExprNode


data Lambda = Lambda {
    _lambdaShouldUpdate :: Bool,
    _lambdaFreeVariables :: [Identifier],
    _lambdaBoundVariables :: [Identifier],
    _lambdaExprNode :: ExprNode
} deriving(Show)

makeLenses ''Lambda


data Binding = Binding {
  _bindingName :: Identifier,
  _bindingLambda :: Lambda
} deriving(Show)

makeLenses ''Binding
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module StgLanguage where

import Control.Monad.Trans.Class
import Control.Lens
import Text.ParserCombinators.Parsec
import Text.PrettyPrint as PP

showStyle :: PP.Style
showStyle = PP.Style {
    mode = PP.PageMode,
    lineLength = 30,
    ribbonsPerLine = 1.5
}

mkNest :: Doc -> Doc
mkNest = nest 4

class Prettyable a where
    mkDoc :: a -> Doc

newtype Identifier = Identifier { _getIdentifier :: String } deriving(Ord, Eq)
makeLenses ''Identifier

instance Show Identifier where
  show ident = "id-" ++ (ident ^. getIdentifier) 

instance Prettyable Identifier where
    mkDoc = text . show

newtype RawNumber = RawNumber { _getRawNumber :: String } 
makeLenses ''RawNumber


instance Show RawNumber where
  show rawnum = "num-" ++ (rawnum ^. getRawNumber)

instance Prettyable RawNumber where
    mkDoc = text . show

data TokenType = TokenTypePlus | 
                 TokenTypeMinus | 
                 TokenTypeDiv | 
                 TokenTypeMultiply | 
                 TokenTypeIdentifier Identifier | 
                 TokenTypeLet |
                 TokenTypeLetrec |
                 TokenTypeIn |
                 TokenTypeCase |
                 TokenTypeOf |
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


instance Show TokenType where
  show TokenTypePlus = "+"
  show TokenTypeMinus = "-"
  show TokenTypeDiv = "/"
  show TokenTypeMultiply = "*"
  show (TokenTypeIdentifier ident)= show ident
  show (TokenTypeRawNumber num) = show num
  show TokenTypeLet = "let"
  show TokenTypeLetrec = "letrec"
  show TokenTypeIn = "in"
  show TokenTypeCase = "case"
  show TokenTypeOf = "of"
  show TokenTypeDefine = "define"
  show (TokenTypeUpdate b) = "\\" ++ (if b then "u" else "n")
  show TokenTypeThinArrow = "->"
  show TokenTypeFatArrow = "=>"
  show TokenTypeEquals = "="
  show TokenTypeSemicolon = ";"
  show TokenTypeOpenBrace = "{"
  show TokenTypeCloseBrace = "}"
  show TokenTypeComma = "}"


instance Prettyable TokenType where
    mkDoc = text . show
  
data Trivia = Trivia {
  _triviaSourcePos :: SourcePos
} deriving(Show)

makeLenses ''Trivia

data Token = Token {
  _tokenType :: TokenType,
  _tokenTrivia :: Trivia
}


instance Prettyable Token where
    mkDoc (Token{..}) = (mkDoc _tokenType)  <+> lparen <> linedoc <> colon <> coldoc <> rparen
                where
                    linedoc = _tokenTrivia ^. triviaSourcePos . to sourceLine . to int
                    coldoc =  _tokenTrivia ^. triviaSourcePos . to sourceColumn . to int

instance (Show Token) where
    show = renderStyle showStyle . mkDoc



data Atom = AtomRawNumber RawNumber | AtomIdentifier Identifier deriving(Show)

instance Prettyable Atom where
    mkDoc (AtomRawNumber n) = mkDoc n
    mkDoc (AtomIdentifier ident) = mkDoc ident

data Binding = Binding {
  _bindingName :: Identifier,
  _bindingLambda :: Lambda
}

data IsLetRecursive = LetRecursive | LetNonRecursive deriving(Show, Eq)


data ExprNode = ExprNodeBinop ExprNode Token ExprNode |
               ExprNodeFnApplication Identifier [Atom] |
               ExprNodeLet IsLetRecursive [Binding] ExprNode |
               ExprNodeCase ExprNode [CaseAlt]

newtype Constructor = Constructor { getConstructor :: String } deriving (Eq, Show)
data CaseAlt = CaseAlt {
  _caseAltConstructor :: Constructor,
  _caseAltVars :: [Identifier],
  _caseAltExpr :: ExprNode
}



data Lambda = Lambda {
    _lambdaShouldUpdate :: Bool,
    _lambdaFreeVarIdentifiers :: [Identifier],
    _lambdaBoundVarIdentifiers :: [Identifier],
    _lambdaExprNode :: ExprNode
} 



instance Prettyable Lambda where
    mkDoc (Lambda{..}) = 
        freedoc <+> updatedoc <+>
        bounddoc <+> text "->" $$
        (mkDoc _lambdaExprNode & mkNest) where
            freedoc = (map mkDoc _lambdaFreeVarIdentifiers) & punctuate comma & hsep & braces
            bounddoc = (map mkDoc _lambdaBoundVarIdentifiers) & punctuate comma & hsep & braces
            updatedoc = PP.char '\\' <> (if _lambdaShouldUpdate then PP.char 'u' else PP.char 'n')

instance Show Lambda where
    show = renderStyle showStyle . mkDoc


makeLenses ''Token
makeLenses ''Binding
makeLenses ''Lambda
makePrisms ''ExprNode
makeLenses ''Atom
makePrisms ''TokenType


instance Prettyable ExprNode where
    mkDoc  (ExprNodeFnApplication fnName atoms) =
        (mkDoc fnName) <+>
        (map mkDoc atoms & punctuate comma & hsep & braces)

    mkDoc (ExprNodeBinop eleft tok eright) = (tok ^. tokenType ^. to mkDoc)  <+>
                                             (mkDoc eleft & mkNest) <+>
                                             (mkDoc eright & mkNest)
    mkDoc (ExprNodeLet isrecursive bindings expr) = 
          letname $$
          mkNest bindingsstr $$ 
          (text "in")  $$
          (expr & mkDoc & mkNest) where
                        letname = PP.text (if isrecursive == LetNonRecursive then "let" else "letrec")
                        bindingsstr = map mkDoc bindings & vcat


instance Show ExprNode where
    show = renderStyle showStyle . mkDoc


instance Prettyable Binding where
    mkDoc (Binding{..}) = (PP.text "define") <+>
                         (mkDoc _bindingName) <+> equals <+>
                         (_bindingLambda & mkDoc)


instance Show Binding where
    show = renderStyle showStyle . mkDoc
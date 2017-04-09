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

instance (Prettyable a, Prettyable b) => Prettyable (a, b) where
  mkDoc (a, b) = braces (mkDoc a PP.<> comma PP.<> mkDoc b)

newtype ConstructorName = ConstructorName { _getConstructorName :: String } deriving (Eq)
makeLenses ''ConstructorName

instance Show ConstructorName where
  show cons = _getConstructorName cons

instance Prettyable ConstructorName where
    mkDoc = text . show
newtype Identifier = Identifier { _getIdentifier :: String } deriving(Ord, Eq)
makeLenses ''Identifier

instance Show Identifier where
  show ident = "ident:" ++ (ident ^. getIdentifier) 

instance Prettyable Identifier where
    mkDoc = text . show

newtype RawNumber = RawNumber { _getRawNumber :: String }  deriving(Eq)
makeLenses ''RawNumber


instance Show RawNumber where
  show rawnum = "num:#" ++ (rawnum ^. getRawNumber)

instance Prettyable RawNumber where
    mkDoc = text . show

data TokenType = TokenTypePlus | 
                 TokenTypeMinus | 
                 TokenTypeDiv | 
                 TokenTypeMultiply | 
                 TokenTypeIdentifier !Identifier | 
                 TokenTypeConstructorName !ConstructorName | 
                 TokenTypeLet |
                 TokenTypeLetrec |
                 TokenTypeIn |
                 TokenTypeCase |
                 TokenTypeOf |
                 TokenTypeDefine |
                 TokenTypeUpdate !Bool | -- true: should update, false: no update
                 TokenTypeThinArrow |
                 TokenTypeFatArrow |
                 TokenTypeSemicolon |
                 TokenTypeOpenBrace |
                 TokenTypeCloseBrace |
                 -- | '('
                 TokenTypeOpenParen |
                 -- | ')'
                 TokenTypeCloseParen |
                 TokenTypeComma |
                 TokenTypeEquals | 
                 TokenTypeRawNumber !RawNumber


instance Show TokenType where
  show TokenTypePlus = "+"
  show TokenTypeMinus = "-"
  show TokenTypeDiv = "/"
  show TokenTypeMultiply = "*"
  show (TokenTypeIdentifier ident) = show ident
  show (TokenTypeConstructorName name) = show name
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
  show TokenTypeOpenParen = "("
  show TokenTypeCloseParen = ")"
  show TokenTypeComma = "}"


instance Prettyable TokenType where
    mkDoc = text . show
  
data Trivia = Trivia {
  _triviaSourcePos :: SourcePos
} deriving(Show)

makeLenses ''Trivia

data Token = Token {
  _tokenType :: !TokenType,
  _tokenTrivia :: !Trivia
}


instance Prettyable Token where
    mkDoc (Token{..}) = (mkDoc _tokenType)  <+> lparen <> linedoc <> colon <> coldoc <> rparen
                where
                    linedoc = _tokenTrivia ^. triviaSourcePos . to sourceLine . to int
                    coldoc =  _tokenTrivia ^. triviaSourcePos . to sourceColumn . to int

instance (Show Token) where
    show = renderStyle showStyle . mkDoc



data Atom = AtomRawNumber !RawNumber | AtomIdentifier !Identifier deriving(Show)

instance Prettyable Atom where
    mkDoc (AtomRawNumber n) = mkDoc n
    mkDoc (AtomIdentifier ident) = mkDoc ident

data Binding = Binding {
  _bindingName :: !Identifier,
  _bindingLambda :: !Lambda
}
type Program = [Binding]


data Constructor = Constructor !ConstructorName ![Identifier]

instance Prettyable Constructor where
  mkDoc (Constructor name idents) = mkDoc name <+> (idents & map mkDoc & hsep)

instance Show Constructor where
  show = renderStyle showStyle . mkDoc

data IsLetRecursive = LetRecursive | LetNonRecursive deriving(Show, Eq)

data ExprNode = ExprNodeBinop !ExprNode !Token !ExprNode |
               ExprNodeFnApplication !Identifier ![Atom] |
               ExprNodeLet !IsLetRecursive ![Binding] !ExprNode |
               ExprNodeCase !ExprNode ![CaseAltType] |
               ExprNodeRawNumber !RawNumber
      

data CaseAlt lhs = CaseAlt {
  _caseAltLHS :: !lhs,
  _caseAltRHS :: !ExprNode
}


instance Prettyable lhs => Prettyable (CaseAlt lhs) where
  mkDoc CaseAlt{..} = mkDoc _caseAltLHS <+>
                      text "->" <+>
                      mkDoc _caseAltRHS

instance Prettyable lhs => Show (CaseAlt lhs) where
    show = renderStyle showStyle . mkDoc

data CaseAltType = -- | match with a constructor: ConstructorName bindNames*
                      CaseAltConstructor !(CaseAlt Constructor) |
                      -- | match with a number: 10 -> e
                      CaseAltRawNumber !(CaseAlt RawNumber) |
                      -- | match with a variable: x -> e
                      CaseAltVariable !(CaseAlt Identifier) 



instance Prettyable CaseAltType where
  mkDoc (CaseAltConstructor altCons) = 
    mkDoc (_caseAltLHS altCons) <+>
    text "->"  <+>
    mkDoc (_caseAltRHS altCons) 

  mkDoc (CaseAltRawNumber altRawNumber) = mkDoc altRawNumber
  mkDoc (CaseAltVariable altIdentifier) = mkDoc altIdentifier

instance Show CaseAltType where
  show = renderStyle showStyle . mkDoc

data Lambda = Lambda {
    _lambdaShouldUpdate :: !Bool,
    _lambdaFreeVarIdentifiers :: ![Identifier],
    _lambdaBoundVarIdentifiers :: ![Identifier],
    _lambdaExprNode :: !ExprNode
} 



instance Prettyable Lambda where
    mkDoc (Lambda{..}) = 
        freedoc <+> updatedoc <+>
        bounddoc <+> text "->" $$
        (mkDoc _lambdaExprNode) where
            freedoc = (map mkDoc _lambdaFreeVarIdentifiers) & punctuate comma & hsep & braces
            bounddoc = (map mkDoc _lambdaBoundVarIdentifiers) & punctuate comma & hsep & braces
            updatedoc = PP.char '\\' <> (if _lambdaShouldUpdate then PP.char 'u' else PP.char 'n')

instance Show Lambda where
    show = renderStyle showStyle . mkDoc


makeLenses ''Token
makeLenses ''Binding
makeLenses ''Lambda
makeLenses ''CaseAlt
makeLenses ''Atom
makePrisms ''ExprNode
makePrisms ''TokenType
makePrisms ''CaseAltType


instance Prettyable ExprNode where
    mkDoc  (ExprNodeFnApplication fnName atoms) =
        (mkDoc fnName) <+>
        (map mkDoc atoms & punctuate comma & hsep & braces)

    mkDoc (ExprNodeBinop eleft tok eright) = (tok ^. tokenType ^. to mkDoc)  <+>
                                             (mkDoc eleft) <+>
                                             (mkDoc eright)
    mkDoc (ExprNodeLet isrecursive bindings expr) = 
          letname $$
          bindingsstr $$ 
          (text "in")  $$
          (expr & mkDoc) where
                        letname = PP.text (if isrecursive == LetNonRecursive then "let" else "letrec")
                        bindingsstr = map mkDoc bindings & vcat

    mkDoc (ExprNodeRawNumber number) = mkDoc number
    mkDoc (ExprNodeCase caseexpr caseAlts) = text "case" <+> mkDoc caseexpr <+> text "of" <+> text "{" $$ (mkNest altsDoc)  $$ text "}" where
                                              altsDoc = fmap mkDoc caseAlts & vcat

instance Show ExprNode where
    show = renderStyle showStyle . mkDoc


instance Prettyable Binding where
    mkDoc (Binding{..}) = (PP.text "define") <+>
                         (mkDoc _bindingName) <+> equals <+>
                         (_bindingLambda & mkDoc)


instance Show Binding where
    show = renderStyle showStyle . mkDoc

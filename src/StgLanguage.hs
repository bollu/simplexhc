{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}

module StgLanguage where

import Control.Monad.Trans.Class
import Control.Lens
-- import Text.PrettyPrint as PP
import ColorUtils
import qualified Data.List.NonEmpty as NE
import Data.Text.Prettyprint.Doc as PP


newtype ConstructorName = ConstructorName { _getConstructorName :: String } deriving (Eq, Ord)
makeLenses ''ConstructorName

instance Show ConstructorName where
  show cons = _getConstructorName cons

instance Pretty ConstructorName where
    pretty = pretty . show

newtype VarName = VarName { _getVariable :: String } deriving(Ord, Eq)

instance Pretty VarName where
    pretty var = mkStyleTag (pretty "var:") <+> (pretty (_getVariable var))

instance Show VarName where
    show = prettyToString

newtype RawNumber = RawNumber { _getRawNumber :: String }  deriving(Eq, Ord)
makeLenses ''RawNumber


instance Pretty RawNumber where
  pretty (RawNumber num) = mkStyleTag (pretty "rawnum:") PP.<> pretty num PP.<> pretty "#"

instance Show RawNumber where
    show = prettyToString

newtype StgInt = StgInt { unStgInt :: Int } deriving(Show, Eq)

instance Pretty StgInt where
  pretty (StgInt i) = pretty . show $ i

data Atom = AtomInt !StgInt | AtomVarName !VarName deriving(Show)

instance Pretty Atom where
    pretty (AtomInt n) = pretty n
    pretty (AtomVarName var) = pretty var

data Binding = Binding {
  _bindingName :: !VarName,
  _bindingLambda :: !Lambda
}
type Program = [Binding]



collectBindingsInExpr :: ExprNode -> [Binding]
collectBindingsInExpr (ExprNodeBinop l _ r) = collectBindingsInExpr l ++ collectBindingsInExpr r
collectBindingsInExpr (ExprNodeFnApplication _ _) = []
collectBindingsInExpr (ExprNodeConstructor _) = []
collectBindingsInExpr (ExprNodeLet _ bindings expr) = (bindings >>= collectBindingsInBinding) ++ collectBindingsInExpr expr
collectBindingsInExpr (ExprNodeCase case' alts) = collectBindingsInExpr case' 
collectBindingsInExpr (ExprNodeInt _ ) = []

collectBindingsInBinding :: Binding -> [Binding]
collectBindingsInBinding b@(Binding _ lambda) = b:(collectBindingsInExpr . _lambdaExprNode $ lambda)

data Constructor = Constructor { _constructorName :: !ConstructorName,
                                 _constructorAtoms :: ![Atom]
                               }

instance Pretty Constructor where
  pretty (Constructor name idents) = pretty name <+> (idents & map pretty & hsep)

instance Show Constructor where
    show = prettyToString


data IsLetRecursive = LetRecursive | LetNonRecursive deriving(Show, Eq)


data BinaryOperator = Plus | Minus | Multiply | Divide deriving(Eq)
instance Show BinaryOperator where
  show Plus = "+"
  show Minus = "-"
  show Multiply = "*"
  show Divide = "/"

instance Pretty BinaryOperator where
    pretty = pretty . show

data ExprNode = ExprNodeBinop !ExprNode !BinaryOperator !ExprNode |
               ExprNodeFnApplication !VarName ![Atom] |
               ExprNodeConstructor !Constructor |
               ExprNodeLet !IsLetRecursive ![Binding] !ExprNode |
               ExprNodeCase !ExprNode ![CaseAltType] |
               ExprNodeInt !StgInt
      


data CaseAlt lhs = CaseAlt {
  _caseAltLHS :: !lhs,
  _caseAltRHS :: !ExprNode
}


instance Pretty lhs => Pretty (CaseAlt lhs) where
  pretty CaseAlt{..} = pretty _caseAltLHS <+>
                      pretty "->" <+>
                      pretty _caseAltRHS

instance Pretty lhs => Show (CaseAlt lhs) where
    show = prettyToString

data ConstructorPatternMatch = ConstructorPatternMatch ConstructorName [VarName] deriving(Eq)

instance Pretty ConstructorPatternMatch where
  pretty (ConstructorPatternMatch consName vars) = 
      pretty consName <+> (fmap pretty vars & punctuate comma & hsep & braces)

data CaseAltType = -- | match with a constructor: ConstructorName bindNames*
                      CaseAltConstructor !(CaseAlt ConstructorPatternMatch) |
                      -- | match with a number: 10 -> e
                      CaseAltInt !(CaseAlt StgInt) |
                      -- | match with a variable: x -> e
                      CaseAltVariable !(CaseAlt VarName) 



instance Pretty CaseAltType where
  pretty (CaseAltConstructor a) = 
    pretty (_caseAltLHS a) <+>
    pretty "->"  <+>
    pretty (_caseAltRHS a) 

  pretty (CaseAltInt a) = pretty a
  pretty (CaseAltVariable a) = pretty a

instance Show CaseAltType where
    show = prettyToString

data Lambda = Lambda {
    _lambdaShouldUpdate :: !Bool,
    _lambdaFreeVarIdentifiers :: ![VarName],
    _lambdaBoundVarIdentifiers :: ![VarName],
    _lambdaExprNode :: !ExprNode
} 



instance Pretty Lambda where
    pretty (Lambda{..}) = 
        freedoc <+> updatedoc <+>
        bounddoc <+> pretty "->" <+>
        (pretty _lambdaExprNode) where
            freedoc = (map pretty _lambdaFreeVarIdentifiers) & punctuate comma & hsep & braces
            bounddoc = (map pretty _lambdaBoundVarIdentifiers) & punctuate comma & hsep & braces
            updatedoc = pretty '\\' <> (if _lambdaShouldUpdate then pretty 'u' else pretty 'n')

instance Show Lambda where
    show = prettyToString


makeLenses ''Binding
makeLenses ''Lambda
makeLenses ''CaseAlt
makeLenses ''Atom
makePrisms ''ExprNode
makePrisms ''CaseAltType
makeLenses ''VarName
makeLenses ''Constructor


instance Pretty ExprNode where
    pretty  (ExprNodeFnApplication fnName atoms) =
        (pretty fnName) <+>
        (map pretty atoms & punctuate comma & hsep & braces)

    pretty (ExprNodeBinop eleft tok eright) = (pretty tok)  <+>
                                             (pretty eleft) <+>
                                             (pretty eright)
    pretty (ExprNodeLet isrecursive bindings expr) = 
          letname <+>
          vcat [bindingsstr,
                (pretty "in"),
                (expr & pretty)] where
                        letname = pretty (if isrecursive == LetNonRecursive then "let" else "letrec")
                        bindingsstr = map pretty bindings & vcat


    pretty (ExprNodeConstructor constructor) = pretty constructor

    pretty (ExprNodeInt n) = pretty n
    pretty (ExprNodeCase e alts) = pretty "case" <+> pretty e <+> pretty "of" <+> pretty "{" <+> (mkNest altsDoc)  <+> pretty "}" where
                                              altsDoc = fmap pretty alts & vcat

instance Show ExprNode where
    show = prettyToString


instance Pretty Binding where
    pretty (Binding{..}) = 
                         (pretty _bindingName) <+> equals <+>
                         (_bindingLambda & pretty)


instance Show Binding where
    show = prettyToString

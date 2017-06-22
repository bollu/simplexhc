{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- http://lpaste.net/355742
-- c_wraith's notes on how to use custom token types.

module StgParser where
import StgLanguage

import Control.Monad (void)

import Data.Set as Set
import Data.List.NonEmpty

import Control.Applicative

import Control.Lens

import Text.Trifecta as TR
import Text.Parser.Token.Highlight 
import Text.Parser.Token.Style
import Text.Trifecta.Delta



import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token

import Data.ByteString.Char8 as BS

import qualified Data.HashSet as HashSet

(<??>) = flip (<?>)

-- | Syntax rules for parsing variable-looking like identifiers.
varId :: IdentifierStyle Parser
varId = IdentifierStyle
    { _styleName = "variable"
    , _styleStart = lower <|> char '_'
    , _styleLetter = alphaNum <|> oneOf "_'#"
    , _styleReserved = HashSet.fromList ["let", "letrec", "in", "case", "of", "default"]
    , _styleHighlight = Identifier
    , _styleReservedHighlight = ReservedIdentifier }

-- | Parse a variable identifier. Variables start with a lower-case letter or
-- @_@, followed by a string consisting of alphanumeric characters or @'@, @_@.
varNamep :: Parser VarName
varNamep = "varname" <??> (VarName <$> (ident varId))


stgIntp :: Parser StgInt
stgIntp = "int" <??> do
  number <- integer
  char '#'
  spaces
  return $ StgInt (fromIntegral number)

constructorNamep :: Parser ConstructorName
constructorNamep = "constructor name" <??> do
  c <- upper
  rest <- many (alphaNum <|> oneOf ['_', '-', '?'])
  spaces
  return $ ConstructorName (c:rest)


updatep :: Parser Bool
updatep = "update token" <??> do
    isUpdatable <- (try (const True <$> symbol "\\u")) <|> (const False <$> symbol "\\n")
    return isUpdatable


atomp :: Parser Atom
atomp = "atom" <??> (identp <|> numberp)
    where
        identp = AtomVarName <$> varNamep
        numberp = AtomInt <$> stgIntp




-- "{" atom1, atom2 .. atomn "}" | {}
atomListp :: Parser [Atom]
atomListp = "atom list" <??> do
  atoms <- braces (sepBy atomp  (symbol ","))
  return atoms

-- Function application: fn_name "{" atom1 "," atom2 ... "}" | {}
applicationp :: Parser ExprNode
applicationp = "application" <??> do
        fn_name <- varNamep
        atoms <- atomListp
        return $ ExprNodeFnApplication fn_name atoms



-- let(rec) parser: ("let" | "letrec") (<binding> ";")+ "in" <expr>
letp :: Parser ExprNode
letp = "let" <??> do
  -- isLetRecursive <- (const LetNonRecursive <$> symbol "let" ) <|> (const LetRecursive <$> symbol "letrec")
  symbol "let"
  let isLetRecursive = LetNonRecursive
  bindings <- sepEndBy1 bindingp (symbol ";")
  symbol "in"
  inExpr <- exprp
  return $ ExprNodeLet isLetRecursive bindings inExpr



caseConstructorAltp :: Parser CaseAltType
caseConstructorAltp = "constructor alt" <??> do
  consname <- constructorNamep
  consvars <- variableListp
  symbol "->"
  rhs <- exprp
  let patternMatch = ConstructorPatternMatch consname consvars
  
  return $ CaseAltConstructor (CaseAlt patternMatch rhs)

caseStgIntAltp :: Parser CaseAltType
caseStgIntAltp = "int alt" <??> do
  num <- stgIntp
  symbol "->"
  rhs <- exprp
  return $ CaseAltInt (CaseAlt num rhs)
  

caseAltp :: Parser CaseAltType
caseAltp = "case alt" <??> (caseConstructorAltp <|> caseStgIntAltp)

casep :: Parser ExprNode
casep = "case" <??> do
  symbol "case"
  e <- exprp
  symbol "of"
  alts <- sepEndBy1 caseAltp (symbol ";")
  return $ ExprNodeCase e alts


parenExprp :: Parser ExprNode
parenExprp = "parenthesised expression" <??> do
          symbol "("
          expr <- exprp
          symbol ")"
          return expr

constructorp :: Parser ExprNode
constructorp = "constructor" <??> do
  consName <- constructorNamep
  params <- atomListp

  return $ ExprNodeConstructor (StgLanguage.Constructor consName params)


exprp :: Parser ExprNode
exprp = "expression" <??>
        (try letp <|>
        try applicationp <|>
        try constructorp <|>
        -- FIXME: factor out "try" into a separate thing
        try casep <|>
        try (ExprNodeInt <$> stgIntp) <|>
        try parenExprp)


-- VarName list: {" id1 "," id2 "," .. idn "} | "{}"
variableListp ::Parser [VarName]
variableListp = "variable name list" <??> do
  braces (sepEndBy varNamep (symbol ","))


-- Lambda form
-- <free vars> <should update> <bound vars> "->" <expr>
-- {x y} \n {z}  -> f x y z
lambdap :: Parser Lambda
lambdap = "lambda form" <??> do
    freeVars <- variableListp
    shouldUpdate <- updatep
    boundVars <- variableListp
    symbol "->"
    rhs <- exprp
    return Lambda {
        _lambdaShouldUpdate = shouldUpdate,
        _lambdaFreeVarIdentifiers = freeVars,
        _lambdaBoundVarIdentifiers = boundVars,
        _lambdaExprNode = rhs
    }


--  <name> = <lambdaform>
bindingp :: Parser Binding
bindingp = "binding" <??> do
  name <- varNamep
  symbol "="
  lambda <- lambdap
  return $ Binding name lambda



stgp :: Parser Program
stgp = sepEndBy1 definep (symbol ";") where
    definep = do
      symbol "define"
      bindingp

parseStg :: String -> Result Program
parseStg string = TR.parseString (spaces *> stgp) (Directed (BS.pack string) 0 0 0 0) string

parseExpr :: String -> Result ExprNode
parseExpr string = TR.parseString (spaces *> exprp) (Directed (BS.pack string) 0 0 0 0) string

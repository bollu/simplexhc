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
import Control.Monad.Error.Hoist

import Text.Trifecta as TR
import Text.Parser.Token.Highlight
import Text.Parser.Token.Style
import Text.Trifecta.Delta



import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token

import Data.ByteString.Char8 as BS



varNamep :: Parser VarName
varNamep = do
    c <- lower
    rest <- many (alphaNum <|> oneOf ['_', '-'])
    possibly_end <- optional (oneOf ['?', '#'])
    return $ VarName $ 
      case possibly_end of 
                Just end -> (c:rest) ++ [end]
                Nothing -> (c:rest)

stgIntp :: Parser StgInt
stgIntp = do
  number <- integer
  char '#'
  return $ StgInt (fromIntegral number)

constructorNamep :: Parser ConstructorName
constructorNamep = do
  c <- upper
  rest <- many (alphaNum <|> oneOf ['_', '-', '?'])
  return $ ConstructorName (c:rest)


updatep :: Parser Bool
updatep = do
    char '\\'
    (const True <$> char 'u') <|> (const False <$> char 'n')


atomp :: Parser Atom
atomp = identp <|> numberp
    where
        identp = AtomVarName <$> varNamep
        numberp = AtomInt <$> stgIntp



-- Parse stuff inside braces, with commas on the inside
-- "{" <stuff> "}"
bracesp :: Parser a -> Parser a
bracesp = between (symbol "{") (symbol "}")

-- "{" atom1, atom2 .. atomn "}" | {}
atomListp :: Parser [Atom]
atomListp = do
  atoms <- bracesp (sepEndBy atomp  (symbol ","))
  return atoms

-- Function application: fn_name "{" atom1 "," atom2 ... "}" | {}
applicationp :: Parser ExprNode
applicationp = do
        fn_name <- varNamep
        atoms <- atomListp
        return $ ExprNodeFnApplication fn_name atoms



-- let(rec) parser: ("let" | "letrec") (<binding> ";")+ "in" <expr>
letp :: Parser ExprNode
letp = do
  isLetRecursive <- (const LetNonRecursive <$> symbol "let" ) <|> (const LetRecursive <$> symbol "letrec")
  bindings <- sepEndBy bindingp (symbol ";")
  symbol "in"
  inExpr <- exprp
  return $ ExprNodeLet isLetRecursive bindings inExpr



caseConstructorAltp :: Parser CaseAltType
caseConstructorAltp = do
  consname <- constructorNamep
  consvars <- variableListp
  symbol "->"
  rhs <- exprp
  let patternMatch = ConstructorPatternMatch consname consvars
  
  return $ CaseAltConstructor (CaseAlt patternMatch rhs)

caseStgIntAltp :: Parser CaseAltType
caseStgIntAltp = do
  num <- stgIntp
  symbol "->"
  rhs <- exprp
  return $ CaseAltInt (CaseAlt num rhs)
  

caseAltp :: Parser CaseAltType
caseAltp = caseConstructorAltp <|> caseStgIntAltp

casep :: Parser ExprNode
casep = do
  symbol "case"
  e <- exprp
  symbol "of"
  alts <- sepEndBy1 caseAltp (symbol ";")
  return $ ExprNodeCase e alts


parenExprp :: Parser ExprNode
parenExprp = do
          symbol "{"
          expr <- exprp
          symbol "}"
          return expr

constructorp :: Parser ExprNode
constructorp = do
  consName <- constructorNamep
  params <- atomListp

  return $ ExprNodeConstructor (StgLanguage.Constructor consName params)


exprp :: Parser ExprNode
exprp = applicationp <|>
        letp <|>
        constructorp <|>
        -- FIXME: factor out "try" into a separate thing
        casep <|>
        (ExprNodeInt <$> stgIntp) <|>
        parenExprp


-- VarName list: {" id1 "," id2 "," .. idn "} | "{}"
variableListp ::Parser [VarName]
variableListp = do
  bracesp (sepEndBy varNamep (symbol ","))


-- Lambda form
-- <free vars> <should update> <bound vars> "->" <expr>
-- {x y} \n {z}  -> f x y z
lambdap :: Parser Lambda
lambdap = do
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
bindingp = do
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
parseStg string = TR.parseString stgp (Directed (BS.pack string) 0 0 0 0) string

parseExpr :: String -> Result ExprNode
parseExpr string = TR.parseString exprp (Directed (BS.pack string) 0 0 0 0) string

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- http://lpaste.net/355742
-- c_wraith's notes on how to use custom token types.

module StgParser where
import StgLanguage

import Control.Monad (void)

import Text.Megaparsec.Char
-- import Text.Megaparsec.String
import Text.Megaparsec as P
import Text.Megaparsec.Pos as P.Pos
import Text.Megaparsec.Expr
import Data.Set as Set
import Data.List.NonEmpty
import qualified Text.Megaparsec.Lexer as L

import Control.Lens
import Control.Monad.Error.Hoist

type StgTokenizer a = Parsec Dec String a

newtype StgTokenStream = StgTokenStream [StgLanguage.Token]

instance  P.Stream StgTokenStream where
  type Token StgTokenStream = StgLanguage.Token

  uncons (StgTokenStream []) = Nothing
  uncons (StgTokenStream (x:xs)) = Just (x, StgTokenStream xs)

  updatePos _ _ current tok = (current, current)

type StgParser a = Parsec Dec StgTokenStream a

sc :: StgTokenizer () -- ‘sc’ stands for “space consumer”
sc = L.space (void spaceChar) lineComment blockComment
  where lineComment  = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"

lexeme :: StgTokenizer a -> StgTokenizer a
lexeme = L.lexeme sc

symbol :: String -> StgTokenizer String
symbol = L.symbol sc

varNameTokenizer :: StgTokenizer VarName
varNameTokenizer = lexeme $ do
    c <- lowerChar
    rest <- many (alphaNumChar <|> oneOf ['_', '-', '?'])
    return $ VarName (c:rest)

numberTokenizer :: StgTokenizer TokenType
numberTokenizer = lexeme $ do
  number <- lexeme L.integer
  hash <- char '#'
  return $ TokenTypeRawNumber (RawNumber . show $ number)

constructorTokenizer :: StgTokenizer TokenType
constructorTokenizer = lexeme $ do
  c <- upperChar
  rest <- many (alphaNumChar <|> oneOf ['_', '-', '?'])
  return $ TokenTypeConstructorName (ConstructorName (c:rest))

identifierLikeTokenizer :: StgTokenizer TokenType
identifierLikeTokenizer = do
    var <- varNameTokenizer
    return $ case var ^. getVariable of
                "define" -> TokenTypeDefine
                "of" -> TokenTypeOf
                "case" -> TokenTypeCase
                "let" -> TokenTypeLet
                "letrec" -> TokenTypeLetrec
                "in" -> TokenTypeIn
                _ -> TokenTypeVarName var

updatetokenizer :: StgTokenizer TokenType
updatetokenizer = do
    char '\\'
    update_or_no <- oneOf ['u', 'n']
    case update_or_no of
        'u' -> return (TokenTypeUpdate True)
        'n' -> return (TokenTypeUpdate False)
        _ -> undefined



glyphTokenizer :: StgTokenizer TokenType
glyphTokenizer = do
    let thinArrow = makeSymbolTokenizer "->" TokenTypeThinArrow
    let fatArrow = makeSymbolTokenizer "=>" TokenTypeFatArrow
    let equals = makeSymbolTokenizer "=" TokenTypeEquals
    let semicolon = makeSymbolTokenizer ";" TokenTypeSemicolon
    let openbrace = makeSymbolTokenizer "{" TokenTypeOpenBrace
    let closebrace = makeSymbolTokenizer "}" TokenTypeCloseBrace
    let openParen = makeSymbolTokenizer "(" TokenTypeOpenParen
    let closeParen = makeSymbolTokenizer ")" TokenTypeCloseParen
    let comma = makeSymbolTokenizer "," TokenTypeComma
    (thinArrow <|> try fatArrow <|>  equals <|> semicolon <|>
     openbrace <|> closebrace <|> comma <|>
     openParen <|> closeParen)
    where
        makeSymbolTokenizer :: String -> TokenType -> StgTokenizer TokenType
        makeSymbolTokenizer str tokenType = fmap (const tokenType) (string str) 


tokenizer :: StgTokenizer StgLanguage.Token
tokenizer = StgLanguage.Token <$> (constructorTokenizer <|> identifierLikeTokenizer <|> numberTokenizer <|> try glyphTokenizer <|> updatetokenizer)

tokenize :: String -> Either (ParseError Char Dec) [StgLanguage.Token]
tokenize input = P.runParser (sc *> many (tokenizer <* sc)) "tokenizer" input


-- Tokens lifted to parsers

istoken :: (TokenType -> Maybe a) -> StgParser a
istoken pred = token test Nothing where
  test x = case pred (_tokenType x) of
             Just a -> Right a
             Nothing -> Left (Set.singleton (Tokens (x:|[])), Set.empty, Set.empty)

  nextpos :: Int -> SourcePos -> StgLanguage.Token -> SourcePos
  nextpos _ pos _ = pos -- HACK: need to fix this


varNamep :: StgParser VarName
varNamep = istoken (^? _TokenTypeVarName)

rawNumberp :: StgParser RawNumber
rawNumberp = istoken (^? _TokenTypeRawNumber)

updatep :: StgParser Bool
updatep = istoken (^? _TokenTypeUpdate)

atomp :: StgParser Atom
atomp = identp <|> numberp
    where
        identp = AtomVarName <$> varNamep
        numberp = AtomRawNumber <$> rawNumberp



semicolonp :: StgParser ()
semicolonp = istoken (^? _TokenTypeSemicolon)

commap :: StgParser ()
commap = istoken (^? _TokenTypeComma)

-- Parse stuff inside braces, with commas on the inside
-- "{" <stuff> "}"
bracesp :: StgParser a -> StgParser a
bracesp = between (istoken (^? _TokenTypeOpenBrace)) (istoken (^? _TokenTypeCloseBrace))

-- "{" atom1, atom2 .. atomn "}" | {}
atomListp :: StgParser [Atom]
atomListp = do
    atoms <- bracesp (sepEndBy atomp  commap)
    return atoms

-- Function application: fn_name "{" atom1 "," atom2 ... "}" | {}
applicationp :: StgParser ExprNode
applicationp = do
        fn_name <- varNamep
        atoms <- atomListp
        return $ ExprNodeFnApplication fn_name atoms

rawnumberp :: StgParser ExprNode
rawnumberp = ExprNodeRawNumber <$> (istoken (^? _TokenTypeRawNumber))

-- let(rec) parser: ("let" | "letrec") <binding>+ "in" <expr>
letp :: StgParser ExprNode
letp = do
  isLetRecursive <- istoken (\case 
                                    TokenTypeLet -> Just LetNonRecursive
                                    TokenTypeLetrec -> Just LetRecursive
                                    _ -> Nothing
                                   )
  bindings <- sepEndBy bindingp semicolonp
  istoken (^? _TokenTypeIn)
  inExpr <- exprp
  return $ ExprNodeLet isLetRecursive bindings inExpr



caseConstructorAltp :: StgParser CaseAltType
caseConstructorAltp = do
  consname <- istoken (^? _TokenTypeConstructorName)  
  consvars <- variableListp
  istoken (^? _TokenTypeThinArrow)
  rhs <- exprp
  let patternMatch = ConstructorPatternMatch consname consvars
  
  return $ CaseAltConstructor (CaseAlt patternMatch rhs)

  

caseConstructorp :: StgParser ExprNode
caseConstructorp = do
  istoken (^? _TokenTypeCase)
  e <- exprp
  istoken (^? _TokenTypeOf)
  alts <- sepEndBy1 caseConstructorAltp semicolonp
  return $ ExprNodeCase e alts


caseRawNumberAltp :: StgParser CaseAltType
caseRawNumberAltp = do
  num <- istoken (^? _TokenTypeRawNumber)
  istoken (^? _TokenTypeThinArrow)
  rhs <- exprp
  return $ CaseAltRawNumber (CaseAlt num rhs)

caseRawNumberp :: StgParser ExprNode
caseRawNumberp = do
  istoken (^? _TokenTypeCase)
  num <- rawnumberp
  istoken (^? _TokenTypeOf)
  alts <- sepEndBy1 caseRawNumberAltp semicolonp
  return $ ExprNodeCase num alts

parenExprp :: StgParser ExprNode
parenExprp = do
          istoken (^? _TokenTypeOpenParen)
          expr <- exprp
          istoken (^? _TokenTypeCloseParen)
          return expr

constructorp :: StgParser ExprNode
constructorp = do
  consName <- istoken (^? _TokenTypeConstructorName)
  params <- atomListp

  return $ ExprNodeConstructor (Constructor consName params)


exprp :: StgParser ExprNode
exprp = applicationp <|>
        letp <|>
        constructorp <|>
        -- FIXME: factor out "try" into a separate thing
        (try caseConstructorp) <|>
        caseRawNumberp <|>
        rawnumberp <|>
        parenExprp


-- VarName list: {" id1 "," id2 "," .. idn "} | "{}"
variableListp ::StgParser [VarName]
variableListp = do
    idents <- bracesp (sepEndBy varNamep  commap)
    return idents


-- Lambda form
-- <free vars> <should update> <bound vars> -> <expr>
-- {x y} \n {z}  -> f x y z
lambdap :: StgParser Lambda
lambdap = do
    freeVars <- variableListp
    shouldUpdate <- updatep
    boundVars <- variableListp
    istoken (^? _TokenTypeThinArrow)
    rhs <- exprp
    return Lambda {
        _lambdaShouldUpdate = shouldUpdate,
        _lambdaFreeVarIdentifiers = freeVars,
        _lambdaBoundVarIdentifiers = boundVars,
        _lambdaExprNode = rhs
    }


--  <name> = <lambdaform>
bindingp :: StgParser Binding
bindingp = do
  -- istoken (^? _TokenTypeDefine)
  name <- varNamep
  istoken (^? _TokenTypeEquals)
  lambda <- lambdap
  return $ Binding name lambda



stgp :: StgParser Program
stgp = sepEndBy1 definep semicolonp where
    definep = do
                istoken (^? _TokenTypeDefine)
                bindingp

parseStg :: [StgLanguage.Token] -> Either (ParseError StgLanguage.Token Dec) Program
parseStg tokens = P.runParser stgp "parserStgProgram" (StgTokenStream tokens)

parseExpr :: [StgLanguage.Token] -> Either (ParseError (P.Token StgTokenStream) Dec) ExprNode
parseExpr tokens = P.runParser exprp "parserStgExpr" (StgTokenStream tokens)

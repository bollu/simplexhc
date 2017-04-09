{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module StgParser where
import StgLanguage
import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import Control.Lens
import Control.Monad.Error.Hoist

type StgTokenizer a = GenParser Char () a 
type StgParser a = GenParser Token () a

identfierTokenizer :: StgTokenizer Identifier
identfierTokenizer = do
    c <- letter
    rest <- many (alphaNum <|> oneOf ['_', '-', '?'])
    return $ Identifier (c:rest)

numberTokenizer :: StgTokenizer TokenType
numberTokenizer = do
  hash <- char '#'
  number_str <- many1 digit
  return $ TokenTypeRawNumber (RawNumber number_str)

constructorTokenizer :: StgTokenizer TokenType
constructorTokenizer = do
  c <- upper
  rest <- many (alphaNum <|> oneOf ['_', '-', '?'])
  return $ TokenTypeConstructorName (ConstructorName (c:rest))

alphanumericTokenizer :: StgTokenizer TokenType
alphanumericTokenizer = do
    ident <- identfierTokenizer
    return $ case ident ^. getIdentifier of
                "define" -> TokenTypeDefine
                "of" -> TokenTypeOf
                "case" -> TokenTypeCase
                "let" -> TokenTypeLet
                "letrec" -> TokenTypeLetrec
                "in" -> TokenTypeIn
                _ -> TokenTypeIdentifier ident

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
    let semicolon = makeSymbolTokenizer ";" TokenTypeSemicolon
    let equals = makeSymbolTokenizer "=" TokenTypeEquals
    let openbrace = makeSymbolTokenizer "{" TokenTypeOpenBrace
    let closebrace = makeSymbolTokenizer "}" TokenTypeCloseBrace
    let openParen = makeSymbolTokenizer "(" TokenTypeOpenParen
    let closeParen = makeSymbolTokenizer ")" TokenTypeCloseParen
    let comma = makeSymbolTokenizer "," TokenTypeComma
    (try thinArrow <|> try fatArrow <|> try equals <|> try semicolon <|>
     try openbrace <|> try closebrace <|> try comma <|>
     try openParen <|> try closeParen)
    where
        makeSymbolTokenizer :: String -> TokenType -> StgTokenizer TokenType
        makeSymbolTokenizer str tokenType = fmap (const tokenType) (string str) 


tokenizer :: StgTokenizer Token
tokenizer = do
  tokenType <- numberTokenizer <|> constructorTokenizer <|> alphanumericTokenizer <|> glyphTokenizer <|> updatetokenizer
  sourcePos <- getPosition
  let trivia = Trivia sourcePos
  return $ Token tokenType trivia

tokenize :: [Char] -> Either ParseError [Token]
tokenize input = parse (many (tokenizer <* spaces)) "tokenizer" input


-- Tokens lifted to parsers

istoken :: (TokenType -> Maybe a) -> StgParser a
istoken pred = tokenPrim show nextpos acceptor where
  nextpos _ token _ = token ^. tokenTrivia . triviaSourcePos
  acceptor token =  token ^. tokenType & pred

identifierp :: StgParser Identifier
identifierp = istoken (^? _TokenTypeIdentifier)

rawNumberp :: StgParser RawNumber
rawNumberp = istoken (^? _TokenTypeRawNumber)

updatep :: StgParser Bool
updatep = istoken (^? _TokenTypeUpdate)

atomp :: StgParser Atom
atomp = identp <|> numberp
    where
        identp = AtomIdentifier <$> identifierp
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
atomsp :: StgParser [Atom]
atomsp = do
    atoms <- bracesp (sepEndBy atomp  commap)
    return atoms

-- Function application: fn_name "{" atom1 "," atom2 ... "}" | {}
applicationp :: StgParser ExprNode
applicationp = do
        fn_name <- identifierp
        atoms <- atomsp
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
  consparams <- many identifierp
  istoken (^? _TokenTypeThinArrow)
  rhs <- exprp
  let constructor = Constructor consname consparams
  return $ CaseAltConstructor ((CaseAlt constructor) rhs)

  

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

exprp :: StgParser ExprNode
exprp = try applicationp <|>
        try letp <|>
        --  try caseConstructorp <|>
        try caseRawNumberp <|>
        try rawnumberp <|>
        try parenExprp


-- Identifier list: {" id1 "," id2 "," .. idn "} | "{}"
identifierListp ::StgParser [Identifier]
identifierListp = do
    idents <- bracesp (sepEndBy identifierp  commap)
    return idents


-- Lambda form
-- <free vars> <should update> <bound vars> -> <expr>
-- {x y} \n {z}  -> f x y z
lambdap :: StgParser Lambda
lambdap = do
    freeVars <- identifierListp
    shouldUpdate <- updatep
    boundVars <- identifierListp
    istoken (^? _TokenTypeThinArrow)
    rhs <- exprp
    return Lambda {
        _lambdaShouldUpdate = shouldUpdate,
        _lambdaFreeVarIdentifiers = freeVars,
        _lambdaBoundVarIdentifiers = boundVars,
        _lambdaExprNode = rhs
    }


-- define <name> = <lambdaform>
bindingp :: StgParser Binding
bindingp = do
  istoken (^? _TokenTypeDefine)
  name <- identifierp
  istoken (^? _TokenTypeEquals)
  lambda <- lambdap
  return $ Binding name lambda



stgParser :: StgParser Program
stgParser = sepEndBy1 bindingp semicolonp

parseStg :: [Token] -> Either ParseError Program
parseStg tokens = parse stgParser "(unknown)" tokens

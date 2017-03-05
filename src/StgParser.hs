{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module StgParser where
import StgLanguage
import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import Control.Lens

type StgTokenizer a = GenParser Char () a 
type StgParser a = GenParser Token () a

identfierTokenizer :: StgTokenizer Identifier
identfierTokenizer = do
    c <- letter
    later <- many (alphaNum <|> oneOf ['_', '-', '?'])
    return $ Identifier (c:later)

numberTokenizer :: StgTokenizer TokenType
numberTokenizer = do
  number_str <- many1 digit
  return $ TokenTypeRawNumber (RawNumber number_str)

alphanumericTokenizer :: StgTokenizer TokenType
alphanumericTokenizer = do
    ident <- identfierTokenizer
    return $ case ident ^. getIdentifier of
                "define" -> TokenTypeDefine
                _ -> TokenTypeIdentifier ident

updatetokenizer :: StgTokenizer TokenType
updatetokenizer = do
    char '\\'
    update_or_no <- oneOf ['u', 'n']
    case update_or_no of
        'u' -> return (TokenTypeUpdate True)
        'n' -> return (TokenTypeUpdate False)
        _ -> undefined



symbolTokenizer :: StgTokenizer TokenType
symbolTokenizer = do
    let thinArrow = makeSymbolTokenizer "->" TokenTypeThinArrow
    let fatArrow = makeSymbolTokenizer "=>" TokenTypeFatArrow
    let semicolon = makeSymbolTokenizer ";" TokenTypeSemicolon
    let equals = makeSymbolTokenizer "=" TokenTypeEquals
    let openbrace = makeSymbolTokenizer "{" TokenTypeOpenBrace
    let closebrace = makeSymbolTokenizer "}" TokenTypeCloseBrace
    let comma = makeSymbolTokenizer ")" TokenTypeCloseBrace
    (thinArrow <|> fatArrow <|> equals <|> semicolon <|>openbrace <|> closebrace <|> comma)
    where
        makeSymbolTokenizer :: String -> TokenType -> StgTokenizer TokenType
        makeSymbolTokenizer str tokenType = fmap (const tokenType) (string str) 


tokenizer :: StgTokenizer Token
tokenizer = do
  tokenType <- numberTokenizer <|> alphanumericTokenizer <|> symbolTokenizer <|> updatetokenizer
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
        numberp = AtomNumber <$> rawNumberp

-- "{" atom1, atom2 .. atomn "}" | {}
atomsp :: StgParser [Atom]
atomsp = do
    let commap = istoken (^? _TokenTypeComma)
    istoken (^? _TokenTypeOpenBrace)
    atoms <- sepBy atomp  commap
    istoken (^? _TokenTypeCloseBrace)
    return atoms

-- Function application: fn_name "{" atom1 "," atom2 ... "}" | {}
applicationp :: StgParser ExprNode
applicationp = do
        fn_name <- identifierp
        atoms <- atomsp
        return $ ExprNodeFnApplication fn_name atoms

-- Expression
exprp :: StgParser ExprNode
exprp = applicationp


-- Identifier list: {" id1 "," id2 "," .. idn "} | "{}"
identifierListp ::StgParser [Identifier]
identifierListp = do
    let commap = istoken (^? _TokenTypeComma)
    istoken (^? _TokenTypeOpenBrace)
    idents <- sepBy identifierp  commap
    istoken (^? _TokenTypeCloseBrace)
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
        _lambdaFreeVariables = freeVars,
        _lambdaBoundVariables = boundVars,
        _lambdaExprNode = rhs
    }


-- define <name> = <lambdaform>
bindingParser :: StgParser Binding
bindingParser = do
  istoken (^? _TokenTypeDefine)
  name <- identifierp
  istoken (^? _TokenTypeEquals)
  lambda <- lambdap
  return $ Binding name lambda


type Program = [Binding]

stgParser :: StgParser Program
stgParser = (\x -> [x]) <$> bindingParser

parseStg :: [Token] -> Either ParseError Program
parseStg tokens = parse stgParser "(unknown)" tokens

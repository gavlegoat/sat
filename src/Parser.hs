module Parser (parseFormula) where

import qualified Data.Map as Map
import Data.Unique
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token

import Types

-- Grammar of formulas:
-- f ::= true | false | STRING | '~' f
--     | f '&' f | f '|' f | f '->' f | f '<->' f

languageDef = emptyDef { Token.identStart = letter
                       , Token.identLetter = alphaNum
                       , Token.reservedNames = ["true", "false"]
                       , Token.reservedOpNames = ["~", "&", "|", "->", "<->"] }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
whiteSpace = Token.whiteSpace lexer

identParser :: ParsecT String VarMap IO Formula
identParser = do
  i <- identifier
  vm <- getState
  case Map.lookup i vm of
    Nothing -> do
      u <- newUnique
      putState (Map.insert u i vm)
      return (Lit $ LVar u)
    Just u -> return (Lit $ LVar u)

term :: ParsecT String VarMap IO Formula
term =   parens formulaParser
     <|> (reserved "true" >> return (Lit LTrue))
     <|> (reserved "false" >> return (Lit LFalse))
     <|> identParser

operators = [ [Prefix (reservedOp "~"   >> return Not    )            ]
            , [Infix  (reservedOp "&"   >> return And    ) AssocLeft  ]
            , [Infix  (reservedOp "|"   >> return Or     ) AssocLeft  ]
            , [Infix  (reservedOp "->"  >> return Implies) AssocRight ,
               Infix  (reservedOp "<->" >> return Iff    ) AssocLeft  ]
            ]

formulaParser :: ParsecT String VarMap IO Formula
formulaParser = buildExpressionParser operators term

parseFormula :: String -> IO Formula
parseFormula input = do
  ret <- runParserT formulaParser Map.empty "input" input
  case ret of
    Left err -> error err
    Right f -> return f

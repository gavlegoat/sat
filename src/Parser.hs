module Parser (parseFormula) where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token

import Types

-- Grammar of formulas:
-- f ::= true | false | STRING | '~' f
--     | f '&' f | f '|' f | f '->' f | f '<->' f

languageDef =
  Token.LanguageDef { Token.commentStart = "/*"
                    , Token.commentEnd = "*/"
                    , Token.commentLine = "//"
                    , Token.nestedComments = False
                    , Token.identStart = letter
                    , Token.identLetter = alphaNum
                    , Token.opStart = oneOf "~&|-<"
                    , Token.opLetter = oneOf "~&|-><"
                    , Token.reservedNames = ["true", "false"]
                    , Token.reservedOpNames = ["~", "&", "|", "->", "<->"]
                    , Token.caseSensitive = False }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
whiteSpace = Token.whiteSpace lexer

identParser :: Parsec String () Formula
identParser = fmap (Lit . LVar) identifier

term :: Parsec String () Formula
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

formulaParser :: Parsec String () Formula
formulaParser = buildExpressionParser operators term

parseFormula :: String -> Formula
parseFormula input =
  case parse formulaParser "input" input of
    Left err -> error $ show err
    Right f -> f

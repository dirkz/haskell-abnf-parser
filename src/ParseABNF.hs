module ParseABNF where

import Text.ParserCombinators.Parsec
import Data.Char (toLower)

data Rule = Rule String Definition
  deriving (Show, Eq)

data Definition = Raw String
  deriving (Show, Eq)

parseABNF :: Parser [Rule]
parseABNF = many parseRule

parseRule :: Parser Rule
parseRule = do
  spaces
  ruleName <- parseRuleName
  spaces
  char '='
  spaces
  rest <- manyTill anyChar newline
  return $ Rule ruleName $ Raw rest

parseRuleName :: Parser String
parseRuleName = map toLower <$> ruleName
  where
    ruleName = (:) <$> letter <*> rest
    rest = many $ alphaNum <|> char '-'

--parseString :: String -> String
parseString = parse parseABNF "debug"

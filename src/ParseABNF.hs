module ParseABNF where

import Text.ParserCombinators.Parsec
import Data.Char (digitToInt, ord)

data RepeatCount = Count Int | Infinity

data Token = Skip -- ^ Just skip/ignore that token
           | Repeat RepeatCount RepeatCount
           | Range Int Int

parseRuleName :: Parser String
parseRuleName = do
  c1 <- (letter <?> "alpha as first char of a rulename")
  cs <- many (letter <|> digit <|> char '-' <?> "alpha-numeric or '-' in rulename")
  return $ c1 : cs

-- | comment
skipComment :: Parser Token
skipComment = char ';' >> many (parseWSP <|> parseVCHAR) >> newline >> return Skip

-- | c-nl
skipCommentOrNewline :: Parser Token
skipCommentOrNewline = skipComment <|> (newline >> return Skip)

-- | repeat
parseRepeat :: Parser Token
parseRepeat = do
  c1 <- maybe (Count 0) Count <$> parseMaybeInt
  char '*'
  c2 <- maybe Infinity Count <$> parseMaybeInt
  return $ Repeat c1 c2

-- | hex-val
parseHexVal :: Parser Token
parseHexVal = do
  char 'x'
  let h1 = many1 parseHEXDIG
  return $ Range 0 0

-- | Base: DIGIT
parseDigit :: Parser Int
parseDigit = digitToInt <$> oneOf ['\x30'..'\x39']

-- | Base: WSP
parseWSP :: Parser Char
parseWSP = char ' ' <|> char '\t'

-- | Base: VCHAR
parseVCHAR :: Parser Char
parseVCHAR = oneOf ['\x21'..'\x7E']

-- | Base: HEXDIG
parseHEXDIG :: Parser Int
parseHEXDIG = parseDigit <|> hexDigit
  where
    minusA = flip (-) $ ord 'A' - 10
    hexDigit = minusA <$> ord <$> oneOf ['A'..'F']

-- | Helper: Parse a list of Ints into an Int, with the given base.
toInt :: Int -> [Int] -> Int
toInt base = fst . foldr fn (0, 1)
  where
    fn x (acc, mul) = (acc + x * mul, mul * base)

-- | Helper: Try to parse a decimal Int
parseMaybeInt :: Parser (Maybe Int)
parseMaybeInt = do
  ds <- many parseDigit
  if null ds then return Nothing
             else return $ Just $ toInt 10 ds

parseDebug :: Parser a -> String -> Either ParseError a
parseDebug parser = parse parser "debug"


module ParseABNF where

import Text.ParserCombinators.Parsec
import Data.Char (digitToInt, ord)

data RepeatCount = Count Int | Infinity
  deriving (Show, Eq, Ord)

data Value = ValueSingle Int
           | ValueSeq [Int]
           | ValueRange Int Int
  deriving (Show, Eq, Ord)

data Token = TokenSkip -- ^ Just skip/ignore that token.
           | TokenRepeat RepeatCount RepeatCount
           | TokenRange Int Int
  deriving (Show, Eq, Ord)

parseRuleName :: Parser String
parseRuleName = do
  c1 <- (letter <?> "alpha as first char of a rulename")
  cs <- many (letter <|> digit <|> char '-' <?> "alpha-numeric or '-' in rulename")
  return $ c1 : cs

-- | comment
skipComment :: Parser Token
skipComment = char ';' >> many (parseWSP <|> parseVCHAR) >> newline >> return TokenSkip

-- | c-nl
skipCommentOrNewline :: Parser Token
skipCommentOrNewline = skipComment <|> (newline >> return TokenSkip)

-- | repeat
parseRepeat :: Parser Token
parseRepeat = do
  c1 <- maybe (Count 0) Count <$> parseMaybeInt
  char '*'
  c2 <- maybe Infinity Count <$> parseMaybeInt
  return $ TokenRepeat c1 c2

-- | Base: bin-val
parseBinVal :: Parser Value
parseBinVal = parseGenericVal parseHEXDIG 2 'b'

-- | Base: dec-val
parseDecVal :: Parser Value
parseDecVal = parseGenericVal parseHEXDIG 10 'd'

-- | Base: hex-val
parseHexVal :: Parser Value
parseHexVal = parseGenericVal parseHEXDIG 16 'x'

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

-- | Helper: hex-val, dec-val, bin-val
parseGenericVal :: Parser Int -> Int -> Char -> Parser Value
parseGenericVal parser base charPrefix = do
  char charPrefix
  h1 <- toInt base <$> many1 parser
  o <- option Nothing $ Just <$> parseDots <|> Just <$> parseDash
  return $
    case o of
      Nothing -> ValueSingle h1
      Just vs@(v:_) ->
        case v of
          (ValueRange h2 _) -> ValueRange h1 h2
          otherwise -> ValueSeq $ h1 : extract vs
  where
    extract :: [Value] -> [Int]
    extract = map extractInt
    extractInt (ValueSingle v) = v
    parseDots = many1 $ parseDotDigits parser base
    parseDash = count 1 (parseDashDigits parser base)

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

-- | Helper: Can parse something like "-" 1*HEXDIG or "." 1*HEXDIG (or DIGIT, BIT).
-- | The HEXDIG depends on the given parser, the result also on the base.
parseDotDashDigits :: Parser Int -> Int -> Char -> Parser Value
parseDotDashDigits parser base c = do
  char c
  h1 <- toInt base <$> many1 parser
  return $ if c == '.' then ValueSingle h1 else ValueRange h1 h1

-- | Helper: Parse "-" 1*HEXDIG (or DIGIT, BIT)
parseDashDigits :: Parser Int -> Int -> Parser Value
parseDashDigits parser base = parseDotDashDigits parser base '-'

-- | Helper: Parse "." 1*HEXDIG (or DIGIT, BIT)
parseDotDigits :: Parser Int -> Int -> Parser Value
parseDotDigits parser base = parseDotDashDigits parser base '.'

-- | Helper: Debug parse a function
parseDebug :: Parser a -> String -> Either ParseError a
parseDebug parser = parse parser "debug"


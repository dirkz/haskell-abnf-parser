module ParseABNF where

import Text.ParserCombinators.Parsec
import Data.Char (digitToInt, ord)

data Comment = Comment String

data RepeatCount = Count Int | Infinity
  deriving (Show, Eq, Ord)

data Value = ValueSingle Int
           | ValueSeq [Int]
           | ValueRange Int Int
           | ValueString String
  deriving (Show, Eq, Ord)

data Repetition = RepeatSingle Int
                | Repeat RepeatCount RepeatCount
  deriving (Show, Eq, Ord)

type RuleName = String

data Definition = DefRef RuleName
                | DefSeq [Definition]
                | DefAlt Definition Definition
                | DefAltAppend Definition
                | DefGroup Definition
                | DefRepeat Repetition Definition
                | DefComment String
                | DefEmpty
  deriving (Show, Eq, Ord)

-- | rulename
parseRuleName :: Parser String
parseRuleName = do
  c1 <- (letter <?> "alpha as first char of a rulename")
  cs <- many (letter <|> digit <|> char '-' <?> "alpha-numeric or '-' in rulename")
  return $ c1 : cs

parseElements :: Parser Definition
parseElements = do
  alt <- parseAlternation
  parseCWSP
  return alt

parseAlternation = undefined

-- | comment
parseComment :: Parser Comment
parseComment = do
  char ';'
  s <- many (parseWSP <|> parseVCHAR)
  newline
  return $ Comment s

-- | c-wsp, comment or whitespace
parseCWSP :: Parser (Maybe Comment)
parseCWSP = try comment <|> (parseWSP >> return Nothing)
  where
   comment = do
    com <- parseComment
    parseWSP
    return $ Just com

-- | c-nl, comment or newline
parseCNL :: Parser (Maybe Comment)
parseCNL = Just <$> parseComment <|> (newline >> return Nothing)

-- | repeat
parseRepeat :: Parser Repetition
parseRepeat = try parseRepeatBoth <|> parseRepeatSingle

parseRepeatSingle :: Parser Repetition
parseRepeatSingle = many1 parseDIGIT >>= return . toInt 10 >>= return . RepeatSingle

parseRepeatBoth :: Parser Repetition
parseRepeatBoth = do
  c1 <- maybe (Count 0) Count <$> parseMaybeInt
  char '*'
  c2 <- maybe Infinity Count <$> parseMaybeInt
  return $ Repeat c1 c2

-- | char-val
parseCharVal :: Parser Value
parseCharVal = do
  char '"'
  s <- many $ oneOf ['\x20'..'\x21'] <|> oneOf ['\x23'..'\x7E']
  char '"'
  return $ ValueString s

-- | Base: num-val
parseNumVal :: Parser Value
parseNumVal = char '%' >> (parseBinVal <|> parseDecVal <|> parseHexVal)

-- | Base: bin-val
parseBinVal :: Parser Value
parseBinVal = parseGenericVal parseBIT 2 'b'

-- | Base: dec-val
parseDecVal :: Parser Value
parseDecVal = parseGenericVal parseDIGIT 10 'd'

-- | Base: hex-val
parseHexVal :: Parser Value
parseHexVal = parseGenericVal parseHEXDIG 16 'x'

-- | Base: DIGIT
parseDIGIT :: Parser Int
parseDIGIT = digitToInt <$> oneOf ['\x30'..'\x39']

-- | Base: WSP
parseWSP :: Parser Char
parseWSP = char ' ' <|> char '\t'

-- | Base: VCHAR
parseVCHAR :: Parser Char
parseVCHAR = oneOf ['\x21'..'\x7E']

-- | Base: BIN
parseBIT :: Parser Int
parseBIT = digitToInt <$> oneOf ['\x30'..'\x31']

-- | Base: HEXDIG
parseHEXDIG :: Parser Int
parseHEXDIG = parseDIGIT <|> hexDigit
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
  ds <- many parseDIGIT
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


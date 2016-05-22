module ParseABNF where

import Text.ParserCombinators.Parsec
import Data.Char (digitToInt, ord)
import Debug.Trace (trace, traceShow, traceShowId)

-- | "=" or "=/"
data DefinedAs = DefinedAs | DefinedAppend
  deriving (Show, Eq, Ord)

data Rule = Rule String DefinedAs Definition
          | RuleEmpty
  deriving (Show, Eq, Ord)

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
                | DefConcat [Definition]
                | DefAlt [Definition]
                | DefAltAppend Definition
                | DefGroup Definition
                | DefRepeat Repetition Definition
                | DefValue Value
  deriving (Show, Eq, Ord)

parseRuleList :: Parser [Rule]
parseRuleList = many1 (try parseRule <|> ws)
  where
    ws = (many (try skipCWSP) >> parseCNL) >> return RuleEmpty

parseRule :: Parser Rule
parseRule = do
  name <- parseRuleNameString <?> "rule name"
  def <- parseDefinedAs <?> "= or =/"
  elm <- parseElements <?> "rule definition"
  parseCNL
  return $ Rule name def elm

-- | rulename
parseRuleName :: Parser Definition
parseRuleName = DefRef <$> parseRuleNameString

-- | Helper: rulename
parseRuleNameString :: Parser String
parseRuleNameString = do
  c1 <- (letter <?> "alpha as first char of a rulename")
  cs <- many $ try (letter <|> digit <|> char '-' <?> "alpha-numeric or '-' in rulename")
  return $ if null cs
              then [c1]
              else c1:cs

parseDefinedAs :: Parser DefinedAs
parseDefinedAs = do
  many skipCWSP
  str <- (string "=" <|> string "=/")
  many skipCWSP
  case str of
    "=" -> return DefinedAs
    "=/" -> return DefinedAppend

skipCWSP :: Parser ()
skipCWSP = skipWSP <|> (parseCNL >> skipWSP)

-- | c-nl, comment or newline
parseCNL :: Parser ()
parseCNL = parseComment <|> (newline >> return ())

parseElements :: Parser Definition
parseElements = do
  alt <- parseAlternation
  many $ try skipCWSP
  return alt

-- | comment
parseComment :: Parser ()
parseComment = do
  char ';'
  many (parseWSP <|> parseVCHAR)
  newline
  return $ ()

-- | Similar to Parsec's `sepBy1`, but uses `try` when trying to parse the
-- | rest, which consists of a separator and another `p`.
abnfSepBy1 :: Parser a -> Parser b -> Parser [a]
abnfSepBy1 p sep = do
  x <- p
  xs <- many $ try rest
  if null xs
     then return [x]
     else return $ x:xs
  where
    rest = sep >> p

-- | Applies `abnfSepBy1` with p and sep, then checks whether the result
-- | is a singleton list. If no, use `multiple` to convert it into a single element.
parseMultiple :: Parser a -> Parser b -> ([a] -> a) -> Parser a
parseMultiple p sep multiple = do
  xs <- abnfSepBy1 p sep
  return $ case xs of
             x:[] -> x
             xs -> multiple xs

parseAlternation :: Parser Definition
parseAlternation = parseMultiple parseConcatenation sep DefAlt
  where
    sep = many skipCWSP >> char '/' >> many skipCWSP

parseConcatenation :: Parser Definition
parseConcatenation = parseMultiple parseRepetition (many1 skipCWSP) DefConcat

parseRepetition :: Parser Definition
parseRepetition = try withRepeat <|> try parseElement <?> "[repeat]element"
  where
    withRepeat = do
      rep <- parseRepeat
      elm <- parseElement
      return $ DefRepeat rep elm

-- | repeat
parseRepeat :: Parser Repetition
parseRepeat = try parseRepeatBoth <|> parseRepeatSingle

-- | Helper: For `parseRepeat`
parseRepeatSingle :: Parser Repetition
parseRepeatSingle = many1 parseDIGIT >>= return . toInt 10 >>= return . RepeatSingle

-- | Helper: For `parseRepeat`
parseRepeatBoth :: Parser Repetition
parseRepeatBoth = do
  c1 <- maybe (Count 0) Count <$> parseMaybeInt
  char '*'
  c2 <- maybe Infinity Count <$> parseMaybeInt
  return $ Repeat c1 c2

-- | elemement
parseElement :: Parser Definition
parseElement = parseRuleName <|> parseGroup <|> parseOption <|>
  DefValue <$> parseCharVal <|> DefValue <$> parseNumVal

-- | Helper: group, option
parseContained :: Char -> Char -> Parser Definition
parseContained start end = do
  char start
  many skipCWSP
  alt <- parseAlternation
  many skipCWSP
  char end
  return alt

-- | group
parseGroup :: Parser Definition
parseGroup = parseContained '(' ')'

-- | option
parseOption :: Parser Definition
parseOption = parseContained '[' ']'

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

skipWSP :: Parser ()
skipWSP = parseWSP >> return ()

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

parseABNF = parse parseRuleList "<parse>"

inputToString :: String -> String
inputToString input =
  case parseABNF input of
    Right rules -> show rules ++ "\n"
    Left err -> show err ++ "\n"

main = interact inputToString

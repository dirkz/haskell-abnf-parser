module ParseABNF where

import Text.ParserCombinators.Parsec
import Data.Char (digitToInt, ord)
import Debug.Trace (trace, traceShow, traceShowId)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (foldl')

import ABNF

type ABNFParser a = GenParser Char () a
type RuleMap = M.Map String Rule

-- | Parses a String, consisting of ABNF rules, into the
-- internal format.
parseABNF :: String -> Either ParseError [Rule]
parseABNF s = rules1 s >>= concatABNF >>= checkConsistency
  where
    filterOut = filter (/= RuleEmpty)
    rules1 = fmap filterOut . runParser parseRuleList () "<parse>"

parseRuleList :: ABNFParser [Rule]
parseRuleList = do
  rules <- many1 (try parseRule <|> ws)
  eof
  return rules
  where
    ws = (many (try skipCWSP) >> parseCNL) >> return RuleEmpty

parseRule :: ABNFParser Rule
parseRule = do
  name <- parseRuleNameString <?> "rule name"
  def <- parseDefinedAs <?> "= or =/"
  elm <- parseElements <?> "rule definition"
  parseCNL
  let rule = Rule name def elm
  return rule

-- | rulename
parseRuleName :: ABNFParser Definition
parseRuleName = DefRef <$> parseRuleNameString

-- | Helper: rulename
parseRuleNameString :: ABNFParser String
parseRuleNameString = do
  c1 <- (letter <?> "alpha as first char of a rulename")
  cs <- many $ try (letter <|> digit <|> char '-' <?> "alpha-numeric or '-' in rulename")
  return $ if null cs
              then [c1]
              else c1:cs

parseDefinedAs :: ABNFParser DefinedAs
parseDefinedAs = do
  many skipCWSP
  str <- (try $ string "=/") <|> string "="
  many skipCWSP
  case str of
    "=" -> return DefinedAs
    "=/" -> return DefinedAppend

skipCWSP :: ABNFParser ()
skipCWSP = skipWSP <|> (parseCNL >> skipWSP)

-- | c-nl, comment or newline
parseCNL :: ABNFParser ()
parseCNL = parseComment <|> (newline >> return ())

parseElements :: ABNFParser Definition
parseElements = do
  alt <- parseAlternation
  many $ try skipCWSP
  return alt

-- | comment
parseComment :: ABNFParser ()
parseComment = do
  char ';'
  many (parseWSP <|> parseVCHAR)
  newline
  return $ ()

-- | Similar to Parsec's `sepBy1`, but uses `try` when trying to parse the
-- rest, which consists of a separator and another `p`.
abnfSepBy1 :: ABNFParser a -> ABNFParser b -> ABNFParser [a]
abnfSepBy1 p sep = do
  x <- p
  xs <- many $ try rest
  if null xs
     then return [x]
     else return $ x:xs
  where
    rest = sep >> p

-- | Applies `abnfSepBy1` with p and sep, then checks whether the result
-- is a singleton list. If no, use `multiple` to convert it into a single element.
parseMultiple :: ABNFParser a -> ABNFParser b -> ([a] -> a) -> ABNFParser a
parseMultiple p sep multiple = do
  xs <- abnfSepBy1 p sep
  return $ case xs of
             x:[] -> x
             xs -> multiple xs

parseAlternation :: ABNFParser Definition
parseAlternation = parseMultiple parseConcatenation sep DefAlt
  where
    sep = many skipCWSP >> char '/' >> many skipCWSP

parseConcatenation :: ABNFParser Definition
parseConcatenation = parseMultiple parseRepetition (many1 skipCWSP) DefConcat

parseRepetition :: ABNFParser Definition
parseRepetition = try withRepeat <|> try parseElement <?> "[repeat]element"
  where
    withRepeat = do
      rep <- parseRepeat
      elm <- parseElement
      return $ DefRepeat rep elm

-- | repeat
parseRepeat :: ABNFParser Repetition
parseRepeat = try parseRepeatBoth <|> parseRepeatSingle

-- | Helper: For `parseRepeat`
parseRepeatSingle :: ABNFParser Repetition
parseRepeatSingle = many1 parseDIGIT >>= return . toInt 10 >>= return . RepeatSingle

-- | Helper: For `parseRepeat`
parseRepeatBoth :: ABNFParser Repetition
parseRepeatBoth = do
  c1 <- maybe (Count 0) Count <$> parseMaybeInt
  char '*'
  c2 <- maybe Infinity Count <$> parseMaybeInt
  return $ Repeat c1 c2

-- | elemement
parseElement :: ABNFParser Definition
parseElement = parseRuleName <|> parseGroup <|> parseOption <|>
  DefValue <$> parseCharVal <|> DefValue <$> parseNumVal

-- | Helper: group, option
parseContained :: Char -> Char -> ABNFParser Definition
parseContained start end = do
  char start
  many skipCWSP
  alt <- parseAlternation
  many skipCWSP
  char end
  return alt

-- | group
parseGroup :: ABNFParser Definition
parseGroup = parseContained '(' ')'

-- | option
parseOption :: ABNFParser Definition
parseOption = parseContained '[' ']'

-- | char-val
parseCharVal :: ABNFParser Value
parseCharVal = do
  char '"'
  s <- many $ oneOf ['\x20'..'\x21'] <|> oneOf ['\x23'..'\x7E']
  char '"'
  return $ ValueString s

-- | Base: num-val
parseNumVal :: ABNFParser Value
parseNumVal = char '%' >> (parseBinVal <|> parseDecVal <|> parseHexVal)

-- | Base: bin-val
parseBinVal :: ABNFParser Value
parseBinVal = parseGenericVal parseBIT 2 'b'

-- | Base: dec-val
parseDecVal :: ABNFParser Value
parseDecVal = parseGenericVal parseDIGIT 10 'd'

-- | Base: hex-val
parseHexVal :: ABNFParser Value
parseHexVal = parseGenericVal parseHEXDIG 16 'x'

-- | Base: DIGIT
parseDIGIT :: ABNFParser Int
parseDIGIT = digitToInt <$> oneOf ['\x30'..'\x39']

-- | Base: WSP
parseWSP :: ABNFParser Char
parseWSP = char ' ' <|> char '\t'

skipWSP :: ABNFParser ()
skipWSP = parseWSP >> return ()

-- | Base: VCHAR
parseVCHAR :: ABNFParser Char
parseVCHAR = oneOf ['\x21'..'\x7E']

-- | Base: BIN
parseBIT :: ABNFParser Int
parseBIT = digitToInt <$> oneOf ['\x30'..'\x31']

-- | Base: HEXDIG
parseHEXDIG :: ABNFParser Int
parseHEXDIG = parseDIGIT <|> hexDigit
  where
    minusA = flip (-) $ ord 'A' - 10
    hexDigit = minusA <$> ord <$> oneOf ['A'..'F']

-- | Helper: hex-val, dec-val, bin-val
parseGenericVal :: ABNFParser Int -> Int -> Char -> ABNFParser Value
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
parseMaybeInt :: ABNFParser (Maybe Int)
parseMaybeInt = do
  ds <- many parseDIGIT
  if null ds then return Nothing
             else return $ Just $ toInt 10 ds

-- | Helper: Can parse something like "-" 1*HEXDIG or "." 1*HEXDIG (or DIGIT, BIT).
-- The HEXDIG depends on the given parser, the result also on the base.
parseDotDashDigits :: ABNFParser Int -> Int -> Char -> ABNFParser Value
parseDotDashDigits parser base c = do
  char c
  h1 <- toInt base <$> many1 parser
  return $ if c == '.' then ValueSingle h1 else ValueRange h1 h1

-- | Helper: Parse "-" 1*HEXDIG (or DIGIT, BIT)
parseDashDigits :: ABNFParser Int -> Int -> ABNFParser Value
parseDashDigits parser base = parseDotDashDigits parser base '-'

-- | Helper: Parse "." 1*HEXDIG (or DIGIT, BIT)
parseDotDigits :: ABNFParser Int -> Int -> ABNFParser Value
parseDotDigits parser base = parseDotDashDigits parser base '.'

-- | Helper: Debug parse a function
parseDebug :: ABNFParser a -> String -> Either ParseError a
parseDebug parser = runParser parser () "debug"

-- | Combine two rules. The first is the existing one.
-- The first rule may not yet exist, which is ok in some cases.
combine :: Maybe Rule -> Rule -> Either String Rule

-- | Combines a '=' defined rule with an existing Nothing.
combine Nothing r@(Rule _ DefinedAs _) = Right r

-- | Combines a '=/' defined rule with an existing Nothing.
combine Nothing (Rule name DefinedAppend _) =
  Left $ "Rule definition '" ++ name ++ " =/': No previous definition to append to"

-- | Combine a '=' defined rule with an already existing rule.
combine (Just (Rule name DefinedAs _)) (Rule _ DefinedAs _) =
  Left $ "Duplicate rule '" ++ name ++ "'"

-- | Combine a '=/' rule with an existing '=' rule.
combine (Just (Rule name DefinedAs (DefAlt rs1))) (Rule _ DefinedAppend (DefAlt rs2)) =
  Right $ Rule name DefinedAs (DefAlt $ rs1 ++ rs2)

-- | Combine a '=/' defined rule with an existing DefAlt rule.
combine (Just (Rule name DefinedAs (DefAlt rs))) (Rule _ DefinedAppend def2) =
  Right $ Rule name DefinedAs (DefAlt $ rs ++ [def2])

-- | Combine a '=/' rule with an existing rule, neither is DefAlt.
combine (Just (Rule name DefinedAs def1)) (Rule _ DefinedAppend def2) =
  Right $ Rule name DefinedAs (DefAlt $ [def1, def2])

-- | Converts a list of rules into a RuleMap (mapping rule names to rules)
rulesMap :: [Rule] -> RuleMap
rulesMap = M.fromList . map kv
  where
    kv rule@(Rule name _ _) = (name, rule)

-- | "Shortcut"
extractRuleRefsFromDefinitions = concat . map extractRuleRefsFromDefinition

-- | Extracts a list of rule references (strings) from a definition
extractRuleRefsFromDefinition :: Definition -> [String]
extractRuleRefsFromDefinition (DefRef s) = [s]
extractRuleRefsFromDefinition (DefConcat defs) = extractRuleRefsFromDefinitions defs
extractRuleRefsFromDefinition (DefAlt defs) = extractRuleRefsFromDefinitions defs
extractRuleRefsFromDefinition (DefAltAppend def) = extractRuleRefsFromDefinitions [def]
extractRuleRefsFromDefinition (DefGroup def) = extractRuleRefsFromDefinitions [def]
extractRuleRefsFromDefinition (DefRepeat _ def) = extractRuleRefsFromDefinitions [def]
extractRuleRefsFromDefinition _ = []

extractRuleRefsFromRule :: Rule -> [String]
extractRuleRefsFromRule (Rule _ _ def) = extractRuleRefsFromDefinition def

-- | Checks a list of rules for consistency, that is, e.g., are all
-- referenced rules acually contained in the rules?
checkConsistency :: [Rule] -> Either ParseError [Rule]
checkConsistency rs = foldr folder (Right []) rs
  where
    ruleMap = rulesMap rs
    folder :: Rule -> Either ParseError [Rule] -> Either ParseError [Rule]
    folder _ (Left s) = Left s
    folder r@(Rule name _ _) (Right rs) =
      let
        refs = extractRuleRefsFromRule r
        notContained = map defined refs
        errors = filter (/= Nothing) notContained
        ok = null errors
        theError = maybe "unknown" id $ head errors
      in
        if ok then Right $ r : rs
              else fail $ "Rule '" ++ name ++ "' referencing undefined rule: '" ++ theError ++ "'"
    defined :: String -> Maybe String
    defined s =
      case M.lookup s ruleMap of
        Nothing -> Just s
        (Just _) -> Nothing


-- | Concatenates rules defined with '=/'
concatABNF :: [Rule] -> Either ParseError [Rule]
concatABNF rs = foldl' folder (Right M.empty) rs >>= return . M.elems
  where
    folder :: Either ParseError RuleMap -> Rule -> Either ParseError RuleMap
    folder (Left e) _ = Left e
    folder (Right m) rule@(Rule name _ _) =
      let
        r1 = M.lookup name m
        r2 = combine r1 rule
      in
        case r2 of
          Right r -> Right $ M.insert name r m
          Left s -> fail s

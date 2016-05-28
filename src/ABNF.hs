module ABNF where

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
                | DefOptional Definition
  deriving (Show, Eq, Ord)

module ProcessABNF where

import ABNF

-- | Cuts the rules away except for those needed to completely define the named rule
pruneABNF :: String -> [Rule] -> Either String [Rule]
pruneABNF rootName rules = Right rules


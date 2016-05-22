module Main where

import Data.List (intercalate)

import ParseABNF

inputToString :: String -> String
inputToString input =
  case parseABNF input of
    Right rules -> rulesString rules ++ "\n"
    Left err -> show err ++ "\n"
  where
    rulesString = intercalate "\n" . map show

main :: IO ()
main = interact inputToString

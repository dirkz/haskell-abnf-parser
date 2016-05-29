module Main where

import Data.List (intercalate)
import System.Console.GetOpt
import Data.Maybe ( fromMaybe )
import System.Environment

import ParseABNF
import ProcessABNF

data Options = Options
  { optRoot :: Maybe String
  } deriving Show

defaultOptions = Options
  { optRoot = Nothing
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option [] ["root-rule", "root"]
    (ReqArg (\r opts -> opts { optRoot = Just r }) "RULENAME")
    "root rule"
  ]

header progName = "Usage: " ++ progName ++ " [OPTION...] <input from STDIN>"

parserOpts :: String -> [String] -> IO (Options, [String])
parserOpts progName argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo (header progName) options))

convertShowEither :: Show e => Either e a -> Either String a
convertShowEither (Right a) = Right a
convertShowEither (Left e) = Left $ show e

processInput :: Options -> String -> IO ()
processInput opts input =
  case convertShowEither (parseABNF input) >>= process of
    Right rules -> putStrLn $ rulesString rules
    Left err -> ioError (userError (err ++ "\n"))
  where
    rulesString = intercalate "\n" . map show
    process rs =
      case optRoot opts of
        Just root -> pruneABNF root rs
        Nothing -> Right rs

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  (opts, files) <- parserOpts progName args
  if null files
    then do
      s <- getContents
      processInput opts s
    else ioError (userError (header progName))

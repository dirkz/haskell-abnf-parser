module Main where

import Data.List (intercalate)
import System.Console.GetOpt
import Data.Maybe ( fromMaybe )
import System.Environment

import ParseABNF

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

processInput :: Options -> String -> IO ()
processInput opts input =
  case parseABNF input of
    Right rules -> putStrLn $ rulesString rules ++ "\n"
    Left err -> ioError (userError (show err ++ "\n"))
  where
    rulesString = intercalate "\n" . map show

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

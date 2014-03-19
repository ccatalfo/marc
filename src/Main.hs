module Main where

import Marc
import Data.List
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  s <- readFile $ head args
  let records = readBatchFromString s
  mapM_ (putStrLn . marcDumpFormat) records

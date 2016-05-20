module Main where

import Marc
import Data.List
import System.Environment
import Options.Applicative

data MarcDumpOptions = MarcDumpOptions
  {
  file :: String
  ,recordNumbersAndOffsets :: Bool
  }

runWithOptions :: MarcDumpOptions -> IO ()
runWithOptions opts = do
  s <- readFile $ file opts
  let records = readBatchFromString s
  mapM_ (putStrLn . marcDumpFormat) records


main :: IO ()
main = execParser opts >>= runWithOptions
  where
    parser = MarcDumpOptions <$> argument str (metavar "file")
     <*> switch (short 'p' <>
                long "print" <>
                help "print numbers")
    opts = info parser (
      fullDesc
      <> progDesc "MARCDUMP utility"
      <> header "MARCDUmp")

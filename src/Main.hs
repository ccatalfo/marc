module Main where
import Marc

main :: IO ()
main =
  interact $ marcDumpFormat . readFromString

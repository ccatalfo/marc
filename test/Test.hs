module Main where

import Test.Tasty (defaultMain,testGroup,TestTree,withResource)
import Test.Tasty.HUnit
import Marc
import System.FilePath.Posix
import System.Environment
import Paths_marc


data MarcFileResource = MarcFileResource String

acquire :: IO MarcFileResource
acquire = do
        file <- getDataFileName "data/record.mrc"
        s <- readFile file
        return $ MarcFileResource s

release :: MarcFileResource -> IO ()
release resource = return ()

testNumberOfFields :: MarcFileResource -> Assertion
testNumberOfFields (MarcFileResource marcString) =
                       let record = readFromString marcString in
                    assertEqual "should have correct number of fields" 20 (length $ mrFields record)

testNumberOfControlFields :: MarcFileResource -> Assertion
testNumberOfControlFields (MarcFileResource marcString) =
                          let record = readFromString marcString
                          in assertEqual "it should have correct number of control fields" 4 (length $ getControlFields record)


main :: IO ()
main = defaultMain $
     withResource acquire release tests

tests :: IO MarcFileResource -> TestTree
tests resource = testGroup "All Tests"
                     [
                     testGroup "Unit Tests"
                           [ testCase "Correct number of fields found" $ resource >>= testNumberOfFields
                             ,testCase "Correct number of control fields found" $ resource >>= testNumberOfControlFields

                            ]
                     ]

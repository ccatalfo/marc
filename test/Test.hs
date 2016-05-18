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
        s <- readFile "test/data/record.mrc"
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

testNumberOfVariableFields :: MarcFileResource -> Assertion
testNumberOfVariableFields (MarcFileResource marcString) =
                          let record = readFromString marcString in assertEqual "it should have correct number of variable fields" 16 (length $ getVariableFields record)

testGetFieldAndSubfield :: MarcFileResource -> Assertion
testGetFieldAndSubfield (MarcFileResource marcString) =
                          let record = readFromString marcString in assertEqual "it should find 245$a" (Just "3DMove") $ (getFieldAndSubfield record "245" 'a')
main :: IO ()
main = defaultMain $
     withResource acquire release singleRecordTests

singleRecordTests :: IO MarcFileResource -> TestTree
singleRecordTests resource = testGroup "Single Record Tests"
                     [
                     testGroup "Unit Tests"
                           [ testCase "Correct number of fields found" $ resource >>= testNumberOfFields
                             ,testCase "Correct number of control fields found" $ resource >>= testNumberOfControlFields
                             ,testCase "Correct number of control fields found" $ resource >>= testNumberOfVariableFields
                             ,testCase "Correct title found" $ resource >>= testGetFieldAndSubfield
                            ]
                      ]

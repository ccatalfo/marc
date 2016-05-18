module Main where

import Test.Tasty (defaultMain,testGroup,TestTree,withResource)
import Test.Tasty.HUnit
import Marc
import System.FilePath.Posix
import System.Environment

data MarcFileResource = MarcFileResource String

acquireSingleRecordFile :: IO MarcFileResource
acquireSingleRecordFile = do
        s <- readFile "test/data/record.mrc"
        return $ MarcFileResource s

acquireMultipleRecordsFile :: IO MarcFileResource
acquireMultipleRecordsFile = do
        s <- readFile "test/data/records303.mrc"
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

testHasfield :: MarcFileResource -> Assertion
testHasfield (MarcFileResource marcString) =
  let record = readFromString marcString in assertBool "hasField should be true" (hasField record "245" == True)

testGetField :: MarcFileResource -> Assertion
testGetField (MarcFileResource marcString) =
  let
    titleField = Marc.Marc21VariableField "245" '0' '0' [Marc.Marc21Subfield 'a' "3DMove",Marc.Marc21Subfield 'h' "[electronic resource]."]
    record = readFromString marcString
    field = getField record "245"
  in assertEqual "getField returns correctly" (Just titleField) $ field

testNumberOfRecords :: MarcFileResource -> Assertion
testNumberOfRecords (MarcFileResource marcString) =
    let records = readBatchFromString marcString in assertEqual "it should have 303 records" 303 $ (length records)

tests :: TestTree
tests = testGroup "Tests" [withResource acquireSingleRecordFile release singleRecordTests, withResource acquireMultipleRecordsFile release multipleRecordTests]

main :: IO ()
main = defaultMain $ tests

singleRecordTests :: IO MarcFileResource -> TestTree
singleRecordTests resource = testGroup "Single Record Tests"
                     [
                     testGroup "Unit Tests"
                           [ testCase "Correct number of fields found" $ resource >>= testNumberOfFields
                             ,testCase "Correct number of control fields found" $ resource >>= testNumberOfControlFields
                             ,testCase "Correct number of control fields found" $ resource >>= testNumberOfVariableFields
                             ,testCase "Correct title found" $ resource >>= testGetFieldAndSubfield
                             ,testCase "hasField returns true" $ resource >>= testHasfield
                             ,testCase "getField returns correct data" $ resource >>= testGetField
                            ]
                      ]

multipleRecordTests :: IO MarcFileResource -> TestTree
multipleRecordTests resource = testGroup "Multiple Record Tests"
  [
    testGroup "Unit Tests"
      [
      testCase "Correct number of records found" $ resource >>= testNumberOfRecords
      ]
  ]

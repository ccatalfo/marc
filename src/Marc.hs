module Marc
    where

import Data.List
import Data.String.Utils
import Data.List.Utils
import Debug.Trace

marc21FieldTerminator = '\x1e'
marc21RecordTerminator = "\x1d"
marc21LeaderLength = 24
marc21DirectoryLength = 12

type Marc21Leader = String
type Marc21BaseAddressOfData = Int

data Marc21DirEntry = Marc21DirEntry {
      mdTag              :: Marc21Tag
      , mdLengthofField  :: Int
      , mdStartPosition  :: Int
}
instance Show Marc21DirEntry where
    show (Marc21DirEntry mdTag mdLengthofField mdStartPosition) = mdTag ++ " " ++ (show mdLengthofField) ++ " " ++ (show mdStartPosition) ++ "\n"

data Marc21Directory = Marc21Directory {
      mdEntries          :: [Marc21DirEntry]
} deriving (Show)

type Marc21SubfieldCode = Char
type Marc21SubfieldValue = String

data Marc21Subfield = Marc21Subfield {
      sfCode                   :: Marc21SubfieldCode,
      sfValue                  :: Marc21SubfieldValue
}
instance Show Marc21Subfield where
    show (Marc21Subfield sfCode sfValue) = "$" ++ [sfCode] ++ sfValue ++ " "


type Marc21Tag = String
type Marc21Indicator = Char

data Marc21Field = Marc21VariableField {
      fTag                     :: Marc21Tag,
      fIndicator1              :: Marc21Indicator,
      fIndicator2              :: Marc21Indicator,
      fSubfields               :: [Marc21Subfield]
    ,fRawData                  :: String
} | Marc21ControlField {
     fTag                      :: Marc21Tag
     ,fValue                    :: String
     , fRawData                :: String
}
instance Show Marc21Field where
    show (Marc21VariableField fTag fIndicator1 fIndicator2 fSubfields fRawData) = fTag ++ " " ++ [fIndicator1] ++ " " ++ [fIndicator2] ++ "  " ++ (concatMap formatSubfield fSubfields)
    show (Marc21ControlField fTag fRawData fValue) = fTag ++ " " ++ " " ++ fValue

formatSubfield :: Marc21Subfield -> String
formatSubfield subfield =
  --"$" ++ [(sfCode (subfields !! 0))] ++ (sfValue (subfields !! 0)) --this works
  "$" ++ [(sfCode subfield)] ++ " " ++ (sfValue subfield) ++ " "

data Marc21Record = Marc21Record {
      mrRawString                :: String,
      mrLeader                   :: Marc21Leader,
      mrDirectory                :: Marc21Directory,
      mrFields                   :: [Marc21Field]
    } deriving (Show)

getControlFields :: Marc21Record -> [Marc21Field]
getControlFields record =
    filter (\field -> startswith "00" (fTag field)) (mrFields record)

getVariableFields :: Marc21Record -> [Marc21Field]
getVariableFields record =
    filter (\field -> not (startswith "00" (fTag field))) (mrFields record)

readFromString :: String -> Marc21Record
readFromString s =
    Marc21Record recordString leader directory fields
    where
        leader = take 24 s
        rlen = read (take 5 s) :: Int
        baseaddress = read( take 5 (drop 12 s)) :: Int
        recordString = take rlen s
        directory = extractDirectory ((split [marc21FieldTerminator] s) !! 0)
        fields = extractFields s baseaddress directory

readBatchFromString :: String -> ([Marc21Record], Int)
readBatchFromString s =
    -- ignore last empty element from split
    let
        records = (init $ split marc21RecordTerminator s)
        numread = length records
    in
    (map readFromString records, numread)

printMarc21Record :: Marc21Record -> String
printMarc21Record r =
    show r

readAndPrint :: String -> String
readAndPrint s =
    show (readFromString s)


printInMarcDumpFormat :: Marc21Record -> IO ()
printInMarcDumpFormat record = do
    putStrLn ("LDR  " ++ (mrLeader record))
    mapM_ print (getControlFields record)
    mapM_ print (getVariableFields record)
    putStrLn " "

{-
    A directory entry in MARC 21 is made up of a tag, length-of-field, and field starting position. The directory begins in character position 24 of the record and ends with a field terminator. It is of variable length and consists of a series of fixed fields, referred to as "entries." One entry is associated with each variable field (control or data) present in the record. Each directory entry is 12 characters in length; the structure of each entry as defined in MARC 21 is represented schematically below. The numbers indicate the character positions occupied by the parts of the entry.

Structure of a Directory Entry in MARC 21 Records
    TAG     LENGTH_OF_FIELD     STARTING_CHARACTER_POSITION
    00-02   03-06               07-11
-}
extractDirectory :: String -> Marc21Directory
extractDirectory s =
    Marc21Directory entries
        where
          rawEntries = in_parts (drop 24 s) marc21DirectoryLength
          entries = map extractDirEntry rawEntries

extractDirEntry :: String -> Marc21DirEntry
extractDirEntry s =
    Marc21DirEntry tag lenOfField startPos
        where
          parts = wholeMap (fixedWidth [3, 4, 5]) s
          tag = parts !! 0
          lenOfField = read (parts !! 1) :: Int
          startPos = read (parts !! 2) :: Int

splitAtMb n l =
    let p = splitAt n l
    in if null $ fst p
       then Nothing
       else Just p

in_parts l n =
    unfoldr (splitAtMb n)
                (l ++ replicate (length l `mod` n) ' ')


extractFields :: String -> Marc21BaseAddressOfData -> Marc21Directory -> [Marc21Field]
extractFields s address directory =
    map (extractControlField s address) controlfields ++ map (extractVariableField s address) variablefields
        where
            controlfields = filter (\entry -> startswith "00" (mdTag entry)) (mdEntries directory)
            variablefields = filter (\entry -> not (startswith "00" (mdTag entry))) (mdEntries directory)


extractVariableField :: String -> Marc21BaseAddressOfData ->  Marc21DirEntry -> Marc21Field
extractVariableField s baseaddress entry =
    let
        len = mdLengthofField entry
        startingPosition = baseaddress + mdStartPosition entry
        rawField = take len (drop startingPosition s)
        indicator1 = rawField !! 0
        indicator2 = rawField !! 1
        subfields = extractSubFields rawField
    in
      Marc21VariableField (mdTag entry) indicator1 indicator2 subfields rawField

extractControlField :: String -> Marc21BaseAddressOfData -> Marc21DirEntry -> Marc21Field
extractControlField s baseaddress entry =
    let
        len = mdLengthofField entry
        startingPosition = baseaddress + mdStartPosition entry
        rawField = drop 1 (take (len-1) (drop startingPosition s))
    in
      Marc21ControlField (mdTag entry) (drop 1 (init rawField)) rawField

extractSubFields :: String -> [Marc21Subfield]
extractSubFields s =
    subfields
    where
      rawSubfields = drop 1 (split "\x1f" (init s))
      createSubfield :: String -> Marc21Subfield
      createSubfield str =
          Marc21Subfield (str !! 0) (drop 1 str)
      subfields = map createSubfield rawSubfields

getField :: Marc21Record -> Marc21Tag -> Maybe Marc21Field
getField record tag  =
  find (\field -> fTag field == tag) (mrFields record)


getFieldAndSubfield :: Marc21Record -> Marc21Tag -> Marc21SubfieldCode -> Maybe Marc21SubfieldValue
getFieldAndSubfield record tag subfieldcode =
  case find (\field -> fTag field == tag) (mrFields record)
       of Nothing -> Nothing
          Just f -> case find (\sf -> sfCode sf == subfieldcode) (fSubfields f)
                    of Nothing -> Nothing
                       Just sf -> Just (sfValue sf)


hasField :: Marc21Record -> Marc21Tag -> Bool
hasField record tag =  tag `elem` (map fTag (mrFields record))

debug :: Show a => a -> a
debug x = trace (show x ++ "\n") x
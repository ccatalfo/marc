module Marc
    where

import Data.List
import Data.String.Utils
import Data.List.Utils
import Debug.Trace

marc21FieldTerminator :: Char
marc21FieldTerminator = '\x1e'

marc21RecordTerminator :: String
marc21RecordTerminator = "\x1d"

marc21LeaderLength :: Int
marc21LeaderLength = 24

marc21DirectoryLength :: Int
marc21DirectoryLength = 12

type Marc21Leader = String
type Marc21BaseAddressOfData = Int

-- |Represents the MARC21 Directory entry (see: https://www.loc.gov/marc/bibliographic/bddirectory.html)
-- Contains a tag, length of field  and starting position in the binary MARC file.
data Marc21DirEntry = Marc21DirEntry {
      mdTag              :: Marc21Tag
      , mdLengthofField  :: Int
      , mdStartPosition  :: Int
}
instance Show Marc21DirEntry where
    show (Marc21DirEntry mdTag mdLengthofField mdStartPosition) = mdTag ++ " " ++ (show mdLengthofField) ++ " " ++ (show mdStartPosition) ++ "\n"

-- |Represents the MARC21 Directory (see: https://www.loc.gov/marc/bibliographic/bddirectory.html)
data Marc21Directory = Marc21Directory {
      mdEntries          :: [Marc21DirEntry]
} deriving (Show)

type Marc21TagCode = String
type Marc21SubfieldCode = Char
type Marc21SubfieldValue = String

-- |Represents a MARC21 subfield (see: https://www.loc.gov/marc/bibliographic/bdsummary.html)
data Marc21Subfield = Marc21Subfield {
      sfCode                   :: Marc21SubfieldCode,
      sfValue                  :: Marc21SubfieldValue
} deriving Eq
instance Show Marc21Subfield where
    show (Marc21Subfield sfCode sfValue) = "$" ++ [sfCode] ++ sfValue ++ " "


type Marc21Tag = String
type Marc21Indicator = Char

-- |Represents a MARC21 Field (see: https://www.loc.gov/marc/bibliographic/bdsummary.html)
-- Can be either a variable field or a control field.
data Marc21Field = Marc21VariableField {
      fTag                     :: Marc21Tag,
      fIndicator1              :: Marc21Indicator,
      fIndicator2              :: Marc21Indicator,
      fSubfields               :: [Marc21Subfield]
} | Marc21ControlField {
     fTag                      :: Marc21Tag
     ,fValue                    :: String
} deriving Eq

instance Show Marc21Field where
    show (Marc21VariableField t i1 i2 sfs) = t ++ " " ++ [i1] ++ " " ++ [i2] ++ "  " ++ (concatMap formatSubfield sfs)
    show (Marc21ControlField t val) = t ++ " " ++ " " ++ val

-- |Returns a formatted MARC21 subfield.
-- e.g. $a 7
formatSubfield :: Marc21Subfield -> String
formatSubfield subfield =
  --"$" ++ [(sfCode (subfields !! 0))] ++ (sfValue (subfields !! 0)) --this works
  "$" ++ [(sfCode subfield)] ++ " " ++ (sfValue subfield) ++ " "

-- |Represents an entire MARC21 record containing Leader, raw string, Directory and list of Marc21Field
data Marc21Record = Marc21Record {
      mrRawString                :: String,
      mrLeader                   :: Marc21Leader,
      mrDirectory                :: Marc21Directory,
      mrFields                   :: [Marc21Field]
    } deriving (Show)

-- |Returns only Marc21ControlField from the Marc21Record.
getControlFields :: Marc21Record -> [Marc21Field]
getControlFields record =
    filter (\field -> startswith "00" (fTag field)) (mrFields record)

-- |Returns on Marc21VariableField from the Marc21Record.
getVariableFields :: Marc21Record -> [Marc21Field]
getVariableFields record =
    filter (\field -> not (startswith "00" (fTag field))) (mrFields record)

-- |Reads a string and attempts to parse it into a Marc21Record.
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

-- |Reads a string and attempts to parse into a list of Marc21Record.
readBatchFromString :: String -> [Marc21Record]
readBatchFromString s =
    -- ignore last empty element from split
    let
        records = (init $ split marc21RecordTerminator s) in
    map readFromString records

printMarc21Record :: Marc21Record -> String
printMarc21Record = show

readAndPrint :: String -> String
readAndPrint s =
    show (readFromString s)

-- |Prints out a Marc21Record in the format returned by the yaz-marcdump utility (see: http://www.indexdata.com/yaz/doc/yaz-marcdump.html)
marcDumpFormat :: Marc21Record -> String
marcDumpFormat record =
    leader ++ "\n" ++ controlFields ++ "\n" ++ variableFields ++ "\n"
    where
      leader = "LDR  " ++ mrLeader record
      controlFields = intercalate "\n" $  map show $ getControlFields record
      variableFields = intercalate "\n" $ map show $ getVariableFields record

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
          rawEntries = inParts (drop 24 s) marc21DirectoryLength
          entries = map extractDirEntry rawEntries

extractDirEntry :: String -> Marc21DirEntry
extractDirEntry s =
    Marc21DirEntry tag lenOfField startPos
        where
          parts = wholeMap (fixedWidth [3, 4, 5]) s
          tag = parts !! 0
          lenOfField = read (parts !! 1) :: Int
          startPos = read (parts !! 2) :: Int

splitAtMb :: Int -> [a] -> Maybe ([a],[a])
splitAtMb n [] = Nothing
splitAtMb n l = Just $ splitAt n l

{- splitAtMb :: Int -> [a] -> Maybe (a)
splitAtMb n l =
    let p = splitAt n l
    in if null $ fst p
       then Nothing
       else Just p
-}
inParts :: [Char] -> Int -> [[Char]]
inParts l n =
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
      Marc21VariableField (mdTag entry) indicator1 indicator2 subfields

extractControlField :: String -> Marc21BaseAddressOfData -> Marc21DirEntry -> Marc21Field
extractControlField s baseaddress entry =
    let
        len = mdLengthofField entry
        startingPosition = baseaddress + mdStartPosition entry
        rawField = drop 1 (take (len-1) (drop startingPosition s))
    in
      Marc21ControlField (mdTag entry) (drop 1 (init rawField))

extractSubFields :: String -> [Marc21Subfield]
extractSubFields s =
    subfields
    where
      rawSubfields = drop 1 (split "\x1f" (init s))
      createSubfield :: String -> Marc21Subfield
      createSubfield str =
          Marc21Subfield (str !! 0) (drop 1 str)
      subfields = map createSubfield rawSubfields

-- |Returns Nothing or the Marc21Field from the given Marc21Record.
-- * Example usage
-- getField record "245"
getField :: Marc21Record -> Marc21Tag -> Maybe Marc21Field
getField record tag  =
  find (\field -> fTag field == tag) (mrFields record)

-- |Returns Nothing or the Marc21Subfield from the given Marc21Record and Marc21Field.
-- * Example usage
-- getSubfield field 'a'
getSubfield :: Marc21Field -> Marc21SubfieldCode -> Maybe Marc21Subfield
getSubfield t sf =
  find (\subfield -> sfCode subfield  == sf) (fSubfields t)

-- |Returns Marc21Field and Marc21Subfield from a given Marc21Record.
-- * Example usage
-- getFieldAndSubfield record "245" 'a'
getFieldAndSubfield :: Marc21Record -> Marc21TagCode -> Marc21SubfieldCode -> Maybe Marc21SubfieldValue
getFieldAndSubfield record tag subfieldcode = do
  field <- getField record tag
  subfield <- getSubfield field subfieldcode
  return $ sfValue subfield


-- |Returns Bool indicating whether the given Marc21Tag is present in the Marc21Record.
-- * Example usage
-- hasField record "245"
hasField :: Marc21Record -> Marc21Tag -> Bool
hasField record tag =  tag `elem` (map fTag (mrFields record))

debug :: Show a => a -> a
debug x = trace (show x ++ "\n") x

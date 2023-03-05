{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import qualified Data.Maybe

type Author = T.Text
type Title = T.Text

data Book = Book { author :: Author, title :: Title } deriving Show

type Html = T.Text

bookToHTML :: Book -> Html
bookToHTML book = mconcat ["<p>\n", titleInTags, authorInTags, "</p>\n"]
    where titleInTags = mconcat ["<strong>", (title book), "</strong>\n"]
          authorInTags = mconcat ["<em>", (author book), "</em>\n"]

booksToHTML :: [Book] -> Html
booksToHTML books = mconcat ["<html>\n"
                            ,"<head><title>books</title></head>"
                            ,"<meta charset='utf-8'/>"
                            ,"</head>\n"
                            ,"<body>\n"
                            ,booksHTML
                            ,"\n</body>\n"
                            ,"</html>"]
    where booksHTML = (mconcat . (map bookToHTML)) books

book1 :: Book
book1 = Book { title = "The Conspiracy Against the Human Race"
               ,author = "Ligotti, Thomas"}

book2 :: Book
book2 = Book { title = "A Short History of Decay"
               ,author = "Cioran Emil"}

book3 :: Book
book3 = Book { title = "The Tears of Eros"
               ,author = "Bataille, Georges"}

myBooks :: [Book]
myBooks = [book1, book2, book3]

type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString

leaderLength :: Int
leaderLength = 24
 
getReader :: MarcRecordRaw -> MarcLeaderRaw
getReader record = B.take leaderLength record

rawToInt :: B.ByteString -> Int
rawToInt = (read . T.unpack . E.decodeUtf8)

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (B.take 5 leader)

nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream = B.splitAt recordLength marcStream
    where recordLength = getRecordLength marcStream

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream = if marcStream == B.empty
                        then []
                        else next : allRecords rest
    where (next, rest) = nextAndRest marcStream

type MarcDirectoryRaw = B.ByteString

-- ベースアドレスを取得する
getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt (B.take 5 remainder)
    where remainder = B.drop 12 leader

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLength afterLeader
    where directoryLength = getDirectoryLength record
          afterLeader = B.drop leaderLength record

type MarcDirectoryEntryRaw = B.ByteString

dirEntryLength :: Int
dirEntryLength = 12

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory = if directory == B.empty
                           then []
                           else nextEntry : splitDirectory restEntries
    where (nextEntry, restEntries) = B.splitAt dirEntryLength directory

data FieldMetaData = FieldMetaData { tag :: T.Text
                                   , fieldLength :: Int
                                   , fieldStart :: Int}

makeFieldMetaData :: MarcDirectoryEntryRaw -> FieldMetaData
makeFieldMetaData entry = FieldMetaData textTag theLength theStart
    where (theTag,rest) = B.splitAt 3 entry
          textTag = E.decodeUtf8 theTag
          (rawLength,rawStart) = B.splitAt 4 rest
          theLength = rawToInt rawLength
          theStart = rawToInt rawStart

getFieldMetaData :: [MarcDirectoryEntryRaw] -> [FieldMetaData]
getFieldMetaData rawEntries = map makeFieldMetaData rawEntries

type FieldText = T.Text

getTextField :: MarcRecordRaw -> FieldMetaData -> FieldText
getTextField record fieldMetaData = E.decodeUtf8 byteStringValue
    where recordLength = getRecordLength record
          baseAddress = getBaseAddress record
          baseRecord = B.drop baseAddress record
          baseAtEntry = B.drop (fieldStart fieldMetaData) baseRecord
          byteStringValue = B.take (fieldLength fieldMetaData) baseAtEntry

main :: IO ()
main = do
    marcData <- B.readFile "sample.mrc"
    let marcRecords = allRecords marcData
    print (length marcRecords)


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

main :: IO ()
main = TIO.writeFile "books.html" (booksToHTML myBooks)
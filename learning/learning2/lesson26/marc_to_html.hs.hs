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

book1 :: Book
book1 = Book { title = "The Conspiracy Against the Human Race"
               ,author = "Ligotti, Thomas"}
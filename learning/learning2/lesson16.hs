-- lesson16
-- 直積型
-- 構造体っぽい
data AuthorName = AuthorName {
    firstName :: String 
    , lastName :: String
}

data Book = Book {
      author    :: Creator
    , isbn      :: String 
    , title     :: String
    , year      :: Int
    , bookPrice :: Double
}

data VinylRecord = VinylRecord {
      artist      :: Creator
    , recordTitle :: String
    , recordYear  :: Int
    , recordPrice :: Double
} 

data CollectableToy = CollectableToy {
      name           :: String
    , description    :: String
    , toyPrice       :: Double
}

-- 直和型
type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName
          | TwoInitialsWithLast Char Char LastName

data Creator = AuthorCreator Author | ArtistCreator Artist
data Author = Author Name
data Artist = Person Name | Band String

hpLovecraft :: Creator
hpLovecraft = AuthorCreator
              (Author (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

data StoreItem = BookItem Book 
               | RecordItem VinylRecord
               | ToyItem CollectableToy
               | Pamph Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (Pamph pamph) = 0

afro :: Creator
afro = ArtistCreator (Band "Afろ")

yurucamp :: StoreItem
yurucamp = BookItem (Book
    afro
    "56161641614"
    "ゆるキャン"
    2022
    790)

-- Practice1
data Pamphlet = Pamphlet {
     pamphTitle          :: String
    ,pamphDescription    :: String
    ,phoneNumber         :: String
}

samplePamph :: StoreItem
samplePamph = Pamph(Pamphlet "title" "setsumei" "999-8888")

-- practice2

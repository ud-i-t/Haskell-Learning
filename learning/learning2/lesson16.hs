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
data Circle = Circle {
    radius :: Double
}

data Square =  Square {
     side :: Double
}

data Rectangle = Rectangle {
     width :: Double
    ,height :: Double
}

data Shape = CircleShape Circle | SquareShape Square | RectangleShape Rectangle

circumference :: Shape -> Double
circumference (CircleShape c) = (radius c)*2*3.14
circumference (SquareShape s) = (side s)*4
circumference (RectangleShape r) = ((width r)+(height r))*2

area :: Shape -> Double
area (CircleShape c) = (radius c)*(radius c)*3.14
area (SquareShape s) = (side s)^2
area (RectangleShape r) = (width r)*(height r)

sampleCircle :: Shape
sampleCircle = CircleShape (Circle 3.0)

sampleSquare :: Shape
sampleSquare = SquareShape (Square 4.0)

sampleRectangle :: Shape
sampleRectangle = RectangleShape (Rectangle 5.0 3.0)
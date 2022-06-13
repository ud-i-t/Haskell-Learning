ifEven myFunction x = if even x
                      then myFunction x
                      else x

getIfEven f = (\x -> ifEven f x)
genIfXCven x = (\f -> ifEven f x) -- evenのときに処理する値をクロージャにする 処理内容を後から渡す

getRequestUrl host apikey resourse id = host ++
                                        "/" ++
                                        resourse ++
                                        "/" ++
                                        id ++
                                        "?token=" ++
                                        apikey

genHostRequestBuilder host = (\apikey resourse id -> getRequestUrl host apikey resourse id) 
exampleUrlBuilder = getRequestUrl "http://example.com"

genApiRequestBuilder hostBuilder apiKey = (\resourse id ->
                                            hostBuilder apiKey resourse id)
myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk311"

genApiHostRequestBuilder hostBuilder apiKey resourse = (\id -> hostBuilder apiKey resourse id)
myExampleUrlBuilder2 = genApiRequestBuilder exampleUrlBuilder "1337hAsk311" "hoge"

add4 a b c d = a + b + c + d

bookIdUrlBuilder = getRequestUrl "http://example.com" "1337hAsk311" "book"

-- lesson4で作った関数
sfOffice name = if lastName < "L"
                then nameText
                     ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText
                     ++ " - PO Box 1010 - San Francisco, CA, 94109"
    where lastName = snd name
          nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
    where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ ": PO Box 456 - Reno, NV, 89523"
    where nameText = snd name

dcOffice name = nameText ++ "- PO Box 5166 - Washington DC, dc, 44941"
    where nameText = (fst name) ++ " " ++ (snd name) ++ "Esq"

getLocationFunction location = case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "dc" -> dcOffice
    _ -> (\name -> (fst name) ++ " " ++ (snd name))

addressLetter name location = locationFunction name
    where locationFunction = getLocationFunction location
-- end

-- 引数が2つの関数の引数をひっくり返す
flipBinaryArgs binaryFunction = (\x y -> binaryFunction y x)

addressLetterV2 = flipBinaryArgs addressLetter
addressLetterNY = addressLetterV2 "ny"

subtract2 = flip (-) 2

-- 練習問題
inc n = n + 1
double n = n * 2
square n = n ^ 2

ifEvenInc = ifEven inc
ifEvenDouble = ifEven double
ifEvenSquare = ifEven square


binaryPartialApplication f x = (\y -> f x y)
ifEven myFunction x = if even x
                      then myFunction x
                      else x

getIfEven f = (\x -> ifEven f x)
double x = x * 2

genIfXCven x = (\f -> ifEven f x) -- evenのときに処理する値をクロージャにする 処理内容を後から渡す


getRequestUrl host apikey resourse id = host ++
                                        "/" ++
                                        resourse ++
                                        "/" ++
                                        id ++
                                        "?token=" ++
                                        apikey

genHostRequestBuilder host = (\apikey resourse id -> getRequestUrl host apikey resourse id) 
exampleUrlBuilder = genHostRequestBuilder "http://example.com"

genApiRequestBuilder hostBuilder apiKey = (\resourse id ->
                                            hostBuilder apiKey resourse id)
myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk311"

genApiHostRequestBuilder hostBuilder apiKey resourse = (\id -> hostBuilder apiKey resourse id)
myExampleUrlBuilder2 = genApiRequestBuilder exampleUrlBuilder "1337hAsk311" "hoge"
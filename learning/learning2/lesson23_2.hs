-- lesson23
{-# LANGUAGE OverloadedStrings #-} -- 言語拡張: Text型にリテラル値を使用
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- 23-3
keyword :: T.Text
keyword = "人間"

bgText :: T.Text
bgText = "生かすことは殺さないことである。生かされているか殺されているかを見分ける力が料理人の力であらねばならぬ。神様が人間に下し給うたとみるべき人間食物の個々の持ち味は、残念でも年を経るに従って、人間の猪口才がすべてを亡ぼしつつあるようだ。例えば砂糖の乱用が、おのおの持つところの異なった「味」を破壊し、本質を滅茶苦茶にしている如き、それである。砂糖さえ入れれば美味いとする今の料理は、極端に味覚の低下を示している。砂糖や「味の素」類品の跋扈ばっこに拍車をかけているのは、料理する者の無定見である。この無定見が、味覚を無神経にし、天然自然によって与えられている個々の美しき「味」に盲目となり、「味」を心に楽しむ世界から葬り去っている。従って、通り一片の栄養学説が栄養受け入れを不充分にしていることは言うまでもない。"

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
  where pieces = T.splitOn query fullText
        highlighted = mconcat ["{",query,"}"]

main = do
  TIO.putStrLn (highlight keyword bgText)
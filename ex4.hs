import           Data.List

newtype Html = Html String deriving Show

createHtml :: String -> Html
createHtml string = Html string

renderHtml :: Html -> String
renderHtml (Html string) = string

makeTextNode :: String -> Html
makeTextNode a =
  if isInfixOf ['<', '>', '=', '&'] a
    then error "Not valid HTML code"
    else createHtml a

makeDiv :: Html -> Html
makeDiv (Html a) = createHtml ("<div>" ++ a ++ "</div>")

makeI :: Html -> Html
makeI (Html a) = createHtml ("<i>" ++ a ++ "</i>")

makeSpan :: Html -> Html
makeSpan (Html a) = createHtml ("<span>" ++ a ++ "</span>")

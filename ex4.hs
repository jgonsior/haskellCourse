--module HtmlLibrary (renderHtml(Html)) where

import Data.List

data Html = Text String | Container { containerTag :: String
                                    , containerAttributes :: [(String,String)]
                                    , containerChildren :: [Html]
                                    } deriving (Show)

data Doctype = Html4 | Html5 | XHtml deriving Show -- Html4 sollte Html heißen
data Document = Document { doctype :: Doctype
                         , headSection :: Html
                         , bodySection :: Html
                         } deriving (Show)

createHtml :: String -> Html
createHtml string = Html string

renderHtml :: Html -> String
renderHtml Container{
                    containerTag=containerTag
                    , containerAttributes=(containerAttribute:containerAttributes)
                    , containerChildren=(containerChild:containerChildren)
                    } = "<" ++ containerTag ++ ">" ++ (renderHtml Container containerTag containerAttributes containerChildren)

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

makeHead :: Html -> Html
makeHead (Html a) = createHtml ("<head>" ++ a ++ "</head>")

makeBody :: Html -> Html
makeBody (Html a) = createHtml ("<body>" ++ a ++ "</body>")


makeDocument :: Doctype -> Document
makeDocument Html4 = Document Html4 (makeHead (Html "")) (makeBody (Html ""))
makeDocument Html5 = Document Html5 (makeHead (Html "")) (makeBody (Html ""))
makeDocument XHtml = Document XHtml (makeHead (Html "")) (makeBody (Html ""))
--renderDocument Html5 =
--renderDocument XHtml =

renderDocument :: Document -> String
renderDocument Document{
                       doctype = Html4
                       , headSection=headSection
                       , bodySection=bodySection
                       } = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" ++ renderHtml(headSection) ++ renderHtml(bodySection)

renderDocument Document{
                      doctype = Html5
                      , headSection=headSection
                      , bodySection=bodySection
                      } = "<!DOCTYPE html>" ++ renderHtml(headSection) ++ renderHtml(bodySection)

renderDocument Document{
                      doctype = XHtml
                      , headSection=headSection
                      , bodySection=bodySection
                      } = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" ++ renderHtml(headSection) ++ renderHtml(bodySection)

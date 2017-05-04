--module HtmlLibrary (renderHtml(Html)) where

import           Data.List

data Html = String | Container { containerTag        :: String
                               , containerAttributes :: [(String,String)]
                               , containerChildren   :: [Html]
                               } deriving (Show)

data Doctype = Html4 | Html5 | XHtml deriving Show -- Html4 sollte Html heißen
data Document = Document { doctype     :: Doctype
                         , headSection :: Html
                         , bodySection :: Html
                         } deriving (Show)

createHtml :: String -> Html
createHtml string = Container{containerTag="", containerChildren=[string]}

renderHtml :: Html -> String
renderHtml Container{
                    containerTag=containerTag
                    , containerAttributes=(containerAttribute:containerAttributes)
                    , containerChildren=(containerChild:containerChildren)
                    } = "<" ++ containerTag ++ ">" ++ (renderHtml Container containerTag containerAttributes containerChildren) ++ "</" ++ containerTag ++ ">"

makeTextNode :: String -> Html
makeTextNode a =
  if isInfixOf ['<', '>', '=', '&'] a
    then error "Not valid HTML code"
    else createHtml a

makeDiv :: Html -> Html
makeDiv a = Container{containerTag="div", containerAttributes="", containerChildren = [a]}

makeI :: Html -> Html
makeI a = Container{containerTag="i", containerAttributes="", containerChildren = [a]}

makeSpan :: Html -> Html
makeSpan a = Container{containerTag="span", containerAttributes="", containerChildren = [a]}

makeHead :: Html -> Html
makeHead a = Container{containerTag="head", containerAttributes="", containerChildren = [a]}

makeBody :: Html -> Html
makeBody a = Container{containerTag="body", containerAttributes="", containerChildren = [a]}


makeDocument :: Doctype -> Document
makeDocument Html4 = Document Html4 (makeHead ("")) (makeBody (""))
makeDocument Html5 = Document Html5 (makeHead ("")) (makeBody (""))
makeDocument XHtml = Document XHtml (makeHead ("")) (makeBody (""))
--renderDocument Html5 =
--renderDocument XHtml =
{-
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
-}

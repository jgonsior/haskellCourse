--module HtmlLibrary (renderHtml(Html)) where

import           Data.List

data Html = Text String | Container  { tag        :: String
                                     , attributes :: [(String,String)]
                                     , children   :: [Html]
                                     } deriving (Show)

data Doctype = Html4 | Html5 | XHtml deriving Show -- Html4 sollte Html heißen
data Document = Document { doctype     :: Doctype
                         , headSection :: Html
                         , bodySection :: Html
                         } deriving (Show)

-- is this needed anymore? and if so, what for?
createHtml :: String -> [(String, String)] -> Html
createHtml string  attributes = Container {
                                          tag=""
                                          , attributes= attributes
                                          , children=[Text string]
                                          }

renderAttributes :: [(String, String)] -> String
renderAttributes [] = " "
renderAttributes ((name, value):attributes) = name ++ "=\"" ++ value ++ "\""
                                              ++ renderAttributes attributes

renderHtml :: Html -> String
renderHtml (Text text) = text
renderHtml Container{tag=""
                    , attributes=[]
                    , children=children
                    } = renderHtmlList children
renderHtml Container{tag=tag
                    , attributes=attributes
                    , children=[]
                    } = "<" ++ tag ++ renderAttributes attributes ++">" ++ "</" ++ tag ++ ">"
renderHtml Container{tag=tag
                     , attributes=attributes
                     , children=children
                     } = "<" ++ tag
                         ++ renderAttributes attributes ++ ">"
                         ++ renderHtmlList children
                         ++ "</" ++ tag ++ ">"

renderHtmlList :: [Html] -> String
renderHtmlList = foldr (\html outputString -> renderHtml html ++ outputString ) ""

makeTextNode :: String -> Html
makeTextNode text =
  if  ['<', '>', '=', '&'] `isInfixOf` text
    then error "Not valid HTML code"
    else Text text

concatHtml :: Html -> Html -> Html
concatHtml a b = Container {tag=""
                           , attributes=[]
                           , children = [a,b]
                            }

makeDiv :: Html -> Html
makeDiv a = Container{tag="div", attributes=[], children = [a]}

makeI :: Html -> Html
makeI a = Container{tag="i", attributes=[], children = [a]}

makeSpan :: Html -> Html
makeSpan a = Container{tag="span", attributes=[], children = [a]}

makeHead :: Html
makeHead = Container{tag="head", attributes=[], children = []}

makeBody :: Html
makeBody = Container{tag="body", attributes=[], children = []}


makeDocument :: Doctype -> Document
makeDocument Html4 = Document Html4 makeHead makeBody
makeDocument Html5 = Document Html5 makeHead makeBody
makeDocument XHtml = Document XHtml makeHead makeBody

--renderDocument Html5 =
--renderDocument XHtml =
renderDocument :: Document -> String
renderDocument Document{
                       doctype = Html4
                       , headSection=headSection
                       , bodySection=bodySection
                       } = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" ++ renderHtml headSection ++ renderHtml bodySection

renderDocument Document{
                      doctype = Html5
                      , headSection=headSection
                      , bodySection=bodySection
                      } = "<!DOCTYPE html>" ++ renderHtml headSection ++ renderHtml bodySection

renderDocument Document{
                      doctype = XHtml
                      , headSection=headSection
                      , bodySection=bodySection
                      } = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" ++ renderHtml headSection ++ renderHtml bodySection

isTextNode :: Html -> Bool
isTextNode (Text _) = True
isTextNode _        = False

hasTag :: String -> Html -> Bool
hasTag askedTag Container{tag=specifiedTag} = askedTag == specifiedTag

module MyHtml where

-- import qualified Network.CGI as CGI
--import Text.XHtml
import Text.XHtml.Transitional
import Extra.List

-- |Build a one row table with no border
onerow cols = table (tr (concatHtml (map td cols)))

noborder = [strAttr  "cellpadding" "0",
            strAttr "cellspacing" "0"]

thinborder = [strAttr "border" "1",
              strAttr  "cellpadding" "0",
              strAttr "cellspacing" "0"]

smallfont html = font html ! [strAttr "size" "1"]

linkTo :: [(String,String)] -> Html -> [(String,String)] -> Html
linkTo cgivars text params =
    anchor text ! [strAttr "href" (showUrl script params)]
    where script = cgiGet cgivars "SCRIPT_NAME"

showUrl :: String -> [(String,String)] -> String
showUrl prefix params = prefix ++ "?" ++ consperse "&" (map (\ (a, b) -> a ++ "=" ++ b) params)

cgiGet :: [(String, String)] -> String -> String
cgiGet cgivar name =
    case lookup name cgivar of
      Nothing -> error ("No such CGI variable: " ++ name)
      Just s -> s

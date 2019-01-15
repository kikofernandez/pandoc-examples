module Main where

import Text.Pandoc.JSON
-- import System.IO.Error (catchIOError)
-- import System.Environment (getEnv)
import Text.Pandoc ()
-- import Data.Text (Text, pack)
import Text.Pandoc.Walk (walk)
import qualified Debug.Trace as T
import Data.Maybe (isJust, fromJust)




start :: Pandoc -> Pandoc
start (Pandoc meta blocks) = Pandoc meta (walk zenformatting blocks)
  where
    zenformatting' (Header x (_, classes, _) inline) =
      [RawBlock (Format "html")
          ("<p class=\"h" ++ show x
            ++ foldl (\acc s -> acc ++ " " ++ s) "" classes
            ++ "\">"),
         Plain inline,
         RawBlock (Format "html") "</p>"
      ]
    zenformatting' HorizontalRule
      | isJust centeredSymbol &&
        ((isMetaInlines.fromJust) centeredSymbol) =
          case fromJust centeredSymbol of
            MetaInlines s -> if (length s == 1) then
                               case s of
                                 [Str symbol] -> return $ Para [Str symbol]
                                 _ -> return HorizontalRule
                             else return HorizontalRule
            _ -> return HorizontalRule
      | otherwise = return HorizontalRule
    zenformatting' (Para inlines) = return $ Para $ concat (map zenformattingInline inlines)
    zenformatting' x = return x

    zenformattingInline (Str s)
      | isJust centeredSymbol &&
        ((isMetaString.fromJust) centeredSymbol) &&
        (s == (getMetaString.fromJust) centeredSymbol)=
           return $ Str $ "<span class=\"centered\">" ++ s ++ "</span>"
      | otherwise = return $ Str s
    zenformattingInline x = return x

    zenformatting [] = []
    zenformatting (h:hs) = zenformatting' h ++ zenformatting hs

    centeredSymbol = lookupMeta "centeredSymbol" meta
    isMetaString (MetaString _) = True
    isMetaString _ = False
    isMetaInlines (MetaInlines _) = True
    isMetaInlines _ = False
    getMetaString (MetaString str) = str
    getMetaString _ = "" -- should be an error





main :: IO ()
main = toJSONFilter start

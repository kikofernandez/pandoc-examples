module Main where

import Text.Pandoc.JSON
-- import System.IO.Error (catchIOError)
-- import System.Environment (getEnv)
import Text.Pandoc ()
-- import Data.Text (Text, pack)
import Text.Pandoc.Walk (walk)
-- import qualified Debug.Trace as T


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
    zenformatting' x = return x

    zenformatting [] = []
    zenformatting (h:hs) = zenformatting' h ++ zenformatting hs

main :: IO ()
main = toJSONFilter start

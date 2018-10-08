module Main where

import Text.Pandoc.JSON
import System.IO.Error (catchIOError)
import System.Environment (getEnv)
import Text.Pandoc
import Data.Text (Text, pack)
import Text.Pandoc.Walk (walk)
import Control.Monad
import qualified Debug.Trace as T

zenformat :: Block -> IO [Block]
zenformat (Header x (_, classes, _) inline) = do
  return $
    [RawBlock (Format "html")
              ("<p class=\"h" ++ show x
                              ++ foldl (\acc s -> acc ++ " " ++ show s) "" classes
                              ++ "\">"),
     Plain inline,
     RawBlock (Format "html") "</p>"]
zenformat x = return $ [T.trace (show x) x]


main :: IO ()
main = do
  toJSONFilter zenformat

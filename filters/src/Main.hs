module Main where

import Text.Pandoc.JSON
import System.IO.Error(catchIOError)
import System.Environment(getEnv)
-- import Debug.Trace

--
-- links that contain the word "note" will be removed from the final output
-- e.g., [note](this text will be removed if we use this filter)
--
-- removeNotes :: Inline -> Inline
-- removeNotes c@(Link (_,_,ls) (Str "note": _) _)
--   | any ((== "t") . fst)  ls = Str ""
--   | otherwise = c
-- removeNotes c = c

removeFenceNotes :: Block -> Block
removeFenceNotes cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "note" namevals of
       Just _     -> Div ("", [], []) []
       Nothing    -> cb
removeFenceNotes x = x

main :: IO ()
main = do
  v <- catchIOError (getEnv "FILTERING_MODE") (\_ -> return "")
  case v of
    "" -> return ()
    "WITH_COMMENTS" -> toJSONFilter removeFenceNotes -- show comments
    "NO_COMMENTS"  -> toJSONFilter removeFenceNotes -- disable comments

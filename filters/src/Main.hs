module Main where

import Text.Pandoc.JSON
import System.IO.Error (catchIOError)
import System.Environment (getEnv)
import Text.Pandoc
--import qualified Data.Text.IO as T
import Text.Pandoc.Walk (walk)
import qualified Data.Text.IO as T
import Data.Text (Text, pack)
-- import Debug.Trace


-- writeMd :: PandocMonad m => Pandoc -> m Text
-- writeMd t = writeMarkdown def t

-- -- --  forall ( m :: * -> *) =>
-- readMd :: PandocMonad m => Text -> m Pandoc
-- readMd t = undefined -- readMarkdown def t

-- mdToMD :: Text -> IO ()
-- mdToMD text =
--   do
--     md <- readMd text -- m Pandoc
--     -- newMd <- walk reinterpretation md
--     result <- writeMd md
--     T.putStrLn result

removeFenceNotes :: Block -> Block
removeFenceNotes cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "note" namevals of
       Just _     -> Div ("", [], []) []
       Nothing    -> cb
removeFenceNotes x = x

reinterpretation :: Block -> Block
reinterpretation cb@(CodeBlock (_, _, namevals) text) =
  case lookup "note" namevals of
    Just _ -> Para [Str text]
    Nothing -> cb
reinterpretation x = x

-- T.getContents >>= mdToMD
main :: IO ()
main = do
  v <- catchIOError (getEnv "FILTERING_MODE") (\_ -> return "")
  case v of
    "WITH_COMMENTS" -> toJSONFilter reinterpretation
    "NO_COMMENTS"  -> toJSONFilter removeFenceNotes -- disable comments
    _ -> return ()

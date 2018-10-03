module Main where

import Text.Pandoc.JSON
import System.IO.Error (catchIOError)
import System.Environment (getEnv)
import Text.Pandoc
import Text.Pandoc.Walk (walk)
import qualified Data.Text.IO as T
import Data.Text (Text, pack)
import qualified Data.Map as M
-- import Debug.Trace


writeMd :: Pandoc -> PandocIO Text
writeMd p = writeMarkdown def p

-- --  forall ( m :: * -> *) =>
readMd :: Text -> PandocIO Pandoc
readMd t = readMarkdown def t

-- mdToMD :: Text -> IO Text
-- mdToMD text = --(runIOorExplode $ readMd) text >>= (runIOorExplode $ writeMd md)
--   do
--     md <- (runIOorExplode $ readMd text)
--     -- newMd <- walk reinterpretation md
--     runIOorExplode $ writeMd md


removeFenceNotes :: Block -> Block
removeFenceNotes cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "note" namevals of
       Just _     -> Div ("", [], []) []
       Nothing    -> cb
removeFenceNotes x = x

defaultMeta :: Meta
defaultMeta = Meta M.empty

reinterpretation :: Block -> IO [Block]
reinterpretation cb@(CodeBlock (_, _, namevals) text) =
  case lookup "note" namevals of
    Just _ -> do
      textMd <- runIOorExplode $ writeMd (Pandoc defaultMeta [Para [Str text]]) -- m Text
      p <- runIOorExplode $ readMd textMd
      case p of
        Pandoc _ b -> return b
    Nothing -> return [cb]
reinterpretation x = return [x]

main = do
  v <- catchIOError (getEnv "FILTERING_MODE") (\_ -> return "")
  case v of
    "WITH_COMMENTS" -> toJSONFilter reinterpretation
    -- "NO_COMMENTS"  -> return $ toJSONFilter removeFenceNotes -- disable comments
    -- _ -> return $ return ()

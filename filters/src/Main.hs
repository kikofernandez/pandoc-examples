module Main where

import Text.Pandoc.JSON
import System.IO.Error (catchIOError)
import System.Environment (getEnv)
import Text.Pandoc
import Text.Pandoc.Walk (walk)
import qualified Data.Text.IO as T
import Data.Text (Text, pack)
-- import Debug.Trace


writeMd :: Pandoc -> PandocIO Text
writeMd p = writeMarkdown def p

-- --  forall ( m :: * -> *) =>
readMd :: Text -> PandocIO Pandoc
readMd t = readMarkdown def t

mdToMD :: Text -> IO ()
mdToMD text =
  do
    md <- runIOorExplode $ readMd text
    -- newMd <- walk reinterpretation md
    result <- runIOorExplode $ writeMd md
    T.putStrLn result

removeFenceNotes :: Block -> Block
removeFenceNotes cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "note" namevals of
       Just _     -> Div ("", [], []) []
       Nothing    -> cb
removeFenceNotes x = x


-- reinterpretation :: Block -> IO Block
-- reinterpretation cb@(CodeBlock (_, _, namevals) text) =
--   case lookup "note" namevals of
--     Just _ -> return $ Para [Str text]
--     Nothing -> return cb
-- reinterpretation x = return x

main = do
  v <- catchIOError (getEnv "FILTERING_MODE") (\_ -> return "")
  case v of
    "WITH_COMMENTS" -> T.getContents >>= mdToMD
    -- "NO_COMMENTS"  -> return $ toJSONFilter removeFenceNotes -- disable comments
    -- _ -> return $ return ()

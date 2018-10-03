module Main where

import Text.Pandoc.JSON
import System.IO.Error (catchIOError)
import System.Environment (getEnv)
import Text.Pandoc
import Data.Text (Text, pack)
import qualified Data.Map as M

writeMd :: Pandoc -> PandocIO Text
writeMd p = writeMarkdown def p

readMd :: Text -> PandocIO Pandoc
readMd t = readMarkdown def t

removeFenceNotes :: Block -> Block
removeFenceNotes cb@(CodeBlock (_, _, namevals) _) =
  case lookup "note" namevals of
       Just _     -> Div ("", [], []) []
       Nothing    -> cb
removeFenceNotes x = x

reinterpretation :: Block -> IO [Block]
reinterpretation cb@(CodeBlock (_, _, namevals) text) =
  case lookup "note" namevals of
    Just _ -> do
      p <- runIOorExplode $ readMd (pack text)
      textMd <- runIOorExplode $ writeMd p
      p' <- runIOorExplode $ readMd textMd
      case p' of
        Pandoc _ b -> return b
    Nothing -> return [cb]
reinterpretation x = return [x]

main = do
  v <- catchIOError (getEnv "FILTERING_MODE") (\_ -> return "")
  case v of
    "WITH_COMMENTS" -> toJSONFilter reinterpretation
    "NO_COMMENTS"  -> toJSONFilter removeFenceNotes -- disable comments
    _ -> return ()

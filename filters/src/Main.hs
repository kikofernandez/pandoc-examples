module Main where

import Text.Pandoc.JSON
import System.IO.Error (catchIOError)
import System.Environment (getEnv)
import Text.Pandoc
import Data.Text (Text, pack)
import Text.Pandoc.Walk (walk)
import Control.Monad

writeMd :: Pandoc -> PandocIO Text
writeMd = writeMarkdown def


readMd :: Text -> PandocIO Pandoc
readMd = readMarkdown def


removeFenceNotes :: Block -> Block
removeFenceNotes cb@(CodeBlock (_, _, namevals) _) =
  case lookup "note" namevals of
       Just _     -> Div ("", [], []) []
       Nothing    -> cb
removeFenceNotes x = x


-- TODO: assumes that there will be a single citation
stringToCitation :: Inline -> Inline
stringToCitation s@(Str t)
  | head t == '@' = Cite [Citation (tail t) [] [] AuthorInText 0 0] [s]
stringToCitation x = x


reinterpretation :: Block -> IO [Block]
reinterpretation cb@(CodeBlock (_, _, namevals) text) =
  case lookup "note" namevals of
    Just _ -> do
      p <- runIOorExplode $ (readMd >=> writeMd >=> readMd) (pack text)
      case walk stringToCitation p of
        Pandoc _ b -> return b
    Nothing -> return [cb]
reinterpretation x = return [x]


main :: IO ()
main = do
  v <- catchIOError (getEnv "FILTERING_MODE") (\_ -> return "")
  case v of
    "WITH_COMMENTS" -> toJSONFilter reinterpretation
    "NO_COMMENTS"  -> toJSONFilter removeFenceNotes -- disable comments
    _ -> return ()

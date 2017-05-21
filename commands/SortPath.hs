module Main where

import Common
import Data.List
import System.Directory
import Data.Ord
import Options.Applicative
import Control.Monad (join)
import Data.Monoid ((<>))
import Data.Time (UTCTime)

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "sort files/directories"
  )
  where
    parser :: Parser (IO ())
    parser =
      work
        <$> argument str
            (  metavar "STRING"
            <> help "sortby"
            )

-- work :: String -> IO ()
-- work a = do
--   input <- lines <$> getContents
--   sortF <- flip sortByM input $ case a of
--         "name" -> (return . id)
--         "time" -> getModificationTime
--         _ -> getModificationTime
--   print sortF

work :: String -> IO ()
work a = do
  input <- lines <$> getContents
  sortF <- case a of
        "name" -> flip sortByM input (return . id         :: FilePath -> IO FilePath)
        "time" -> flip sortByM input (getModificationTime :: FilePath -> IO UTCTime)
        _ ->      flip sortByM input (getModificationTime :: FilePath -> IO UTCTime)
  printRaw sortF

sortByM :: (Monad m, Ord a) => (b-> m a) -> [b] -> m [b]
sortByM f x = do
  x' <- mapM f x
  return $ fst <$> (sortBy (comparing snd) $ zip x x')

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import Common
import Control.Monad (join)
import Data.Monoid ((<>))

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "list directories"
  )
  where
    parser :: Parser (IO ())
    parser =
      work
        <$> argument str
            (  metavar "STRING"
            <> help "string parameter"
            )
        <*> option auto
            (  long "sort"
            <> short 's'
            <> metavar "NUMBER"
            <> help "number parameter"
            <> value "name"
            <> showDefault
            )

  -- let opts = Commands <$> target <*> (target <|> target)
  -- erm <- execParser $ info (opts <**> helper) (fullDesc <> progDesc "hmm")
  -- case erm of
  --   (Commands a' b') -> do
  --     print a'
  --     print b'
  --     dirs <- getDirectories a'
  --     mapM_ putStrLn dirs

work :: String -> String -> IO ()
work a _ = getDirectories a >>= printRaw

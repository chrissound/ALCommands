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
        <*> switch
            (  long "full-path"
            <> short 'f'
            <> help ""
            <> showDefault
            )
        <*> switch
            (  long "relative-path"
            <> short 'r'
            <> help ""
            <> showDefault
            )

work :: String -> Bool -> Bool -> IO ()
work a True False =  getDirectoriesFullPath a >>= printRaw
work a False True = getDirectoriesRelativePath a >>= printRaw
work a True True = error "-f and -r is invalid"

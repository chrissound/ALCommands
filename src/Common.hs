{-# LANGUAGE OverloadedStrings #-}
module Common where

import           Control.Monad         (filterM)
import           System.Directory      (doesDirectoryExist, listDirectory)
import           System.FilePath.Posix ((</>))

getDirectories :: FilePath -> IO [FilePath]
getDirectories filePath = listDirectory filePath
                      >>= filterM (doesDirectoryExist . (filePath </>))


printRaw :: Foldable f => f String -> IO ()
printRaw = mapM_ putStrLn

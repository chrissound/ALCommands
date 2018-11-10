{-# LANGUAGE OverloadedStrings #-}
module Common where

import           Control.Monad         (filterM)
import           System.Directory      (doesDirectoryExist, listDirectory, canonicalizePath)
import           System.FilePath.Posix ((</>))

getDirectories :: FilePath -> IO [FilePath]
getDirectories filePath = listDirectory filePath
                      >>= filterM (doesDirectoryExist . (filePath </>))

getDirectoriesRelativePath :: FilePath -> IO [FilePath]
getDirectoriesRelativePath fp = (fmap . fmap ) (fp </>) $ getDirectories fp

getDirectoriesFullPath :: FilePath -> IO [FilePath]
getDirectoriesFullPath fp = canonicalizePath fp >>= (\fp' -> (fmap . fmap ) (fp' </>) $ getDirectories fp)

printRaw :: Foldable f => f String -> IO ()
printRaw = mapM_ putStrLn

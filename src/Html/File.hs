module Html.File
  ( deleteAllFilesInDirectory
  ) where

import System.Exit (ExitCode (ExitFailure), ExitCode(ExitSuccess))
import System.Directory

deleteAllFilesInDirectory :: FilePath -> IO ExitCode
deleteAllFilesInDirectory directory = do
  isDirectoryExist <- doesDirectoryExist directory
  if isDirectoryExist
     then do
       files <- listDirectory directory
       foldMap (removeFileInDirectory directory) files
       return ExitSuccess
  else doWhenDirectoryNotFound directory
    where
      removeFileInDirectory directory file = removeFile $ directory <> file

doWhenDirectoryNotFound :: FilePath -> IO ExitCode
doWhenDirectoryNotFound directory = do
  putStrLn $ "Directory " <> directory <> " not found."
  return $ ExitFailure 1


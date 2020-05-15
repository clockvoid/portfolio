module File
  (
  ) where

import System.Exit (ExitCode (ExitFailure), ExitCode(ExitSuccess))
import System.Directory

deleteAllFilesInDirectory :: FilePath -> IO ExitCode
deleteAllFilesInDirectory directory = do
  isFileExist <- doesDirectoryExist directory
  if isFileExist
     then do
       files <- listDirectory directory
       foldMap removeFile files
       return ExitSuccess
  else return $ ExitFailure 1

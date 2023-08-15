module Html.Lib
  ( deployDirectory,
    templatesDirectory,
    pages,
    compile,
    deleteAllFilesInDirectory,
  )
where

import Html.Article
import Html.Blog
import Html.BlogPost
import Html.Index
import Lucid
import System.Directory
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

deployDirectory :: FilePath
deployDirectory = "_html/"

templatesDirectory :: FilePath
templatesDirectory = "templates/"

pages :: [(FilePath, Html ())]
pages =
  [ indexPage,
    blogTemplate,
    blogPostTemplate,
    articleTemplate
  ]

compile :: FilePath -> [(FilePath, Html ())] -> IO ExitCode
compile directory pages = do
  createDirectoryIfMissing False directory
  createDirectoryIfMissing False $ directory <> templatesDirectory
  foldMap (writePage directory) pages >> ok
  where
    writePage directory (path, page) = do
      renderToFile (directory <> path) page
      putStrLn $ "updated " <> directory <> path
    ok = return ExitSuccess

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
    removeFileInDirectory directory file = do
      putStrLn $ "Deleting... " <> directory <> file
      removeFile $ directory <> file

doWhenDirectoryNotFound :: FilePath -> IO ExitCode
doWhenDirectoryNotFound directory = do
  putStrLn $ "Directory " <> directory <> " not found."
  return $ ExitFailure 1

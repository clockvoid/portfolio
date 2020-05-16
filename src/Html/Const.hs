{-# LANGUAGE OverloadedStrings #-}

module Html.Const
  ( deployDirectory
  , indexPage
  , compile
  ) where

import Data.Text
import System.Directory
import System.Exit (ExitCode (ExitFailure), ExitCode (ExitSuccess))

import Lucid

deployDirectory :: FilePath
deployDirectory = "_html/"

indexPath :: FilePath
indexPath = "index.html"

indexHtml :: Html ()
indexHtml = do
  doctype_
  html_ $ do
    head_ $ do
      title_ "clockvoid - $title$"
      meta_ [charset_ "utf-8"]
      meta_ [httpEquiv_ "x-ua-compatible", content_ "ie=edge"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      link_ [rel_ "stylesheet", href_ "/css/default.css"]

indexPage :: (FilePath, Html ())
indexPage = (indexPath, indexHtml)

compile :: FilePath -> [(FilePath, Html ())] -> IO ExitCode
compile directory pages = do
  createDirectoryIfMissing False directory
  foldMap (writePage directory) pages >> ok
  where
    writePage directory (path, page) = renderToFile (directory <> path) page
    ok = return ExitSuccess


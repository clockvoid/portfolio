{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.List (isPrefixOf, isSuffixOf)
import Data.Monoid (mappend)
import Hakyll
import Hakyll.Web.Sass (sassCompiler)
import Html.Lib qualified as HL
import Site.About
import Site.Blog
import Site.Images
import Site.Index
import Site.Lib
import Site.Posts
import Site.Works
import System.FilePath (takeFileName)
import System.Process

myIgnoreFile :: FilePath -> Bool
myIgnoreFile path
  | "." `isPrefixOf` fileName = True
  | "#" `isPrefixOf` fileName = True
  | "~" `isSuffixOf` fileName = True
  | ".swp" `isSuffixOf` fileName = True
  | "package.json" == fileName = True
  | "node_modules" == fileName = True
  | "src" == fileName = True
  | "app" == fileName = True
  | otherwise = False
  where
    fileName = takeFileName path

main :: IO ()
main = hakyllWith defaultConfiguration {ignoreFile = myIgnoreFile} $ do
  images
  posts
  blog
  index
  about
  works

  match "_html/templates/*" $ compile templateBodyCompiler
  match "css/*" $ do
    route $ setExtension "css"
    let compressCssItem = fmap compressCss
    compile (compressCssItem <$> sassCompiler)

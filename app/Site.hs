{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll

import System.FilePath (takeFileName)
import Data.List (isPrefixOf, isSuffixOf)

import Site.Images
import Site.Css
import Site.Posts
import Site.Archive
import Site.Index
import Site.Lib
import qualified Html.Lib as HL

myIgnoreFile :: FilePath -> Bool
myIgnoreFile path 
  | "."    `isPrefixOf` fileName = True
  | "#"    `isPrefixOf` fileName = True
  | "~"    `isSuffixOf` fileName = True
  | ".swp" `isSuffixOf` fileName = True
  | "node_modules" ==   fileName = True
  | otherwise                    = False
  where
    fileName = takeFileName path

main :: IO ()
main = do
  HL.compile HL.deployDirectory HL.pages
  hakyllWith defaultConfiguration {ignoreFile = myIgnoreFile} $ do
    images
    css
    posts
    archive
    index

    --match (fromList ["about.rst", "contact.markdown"]) $ do
    --    route   $ setExtension "html"
    --    compile $ pandocCompiler
    --        >>= loadAndApplyTemplate "templates/default.html" defaultContext
    --        >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

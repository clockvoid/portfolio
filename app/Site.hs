{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll

import System.FilePath (takeFileName)
import System.Process
import Data.List (isPrefixOf, isSuffixOf)

import Site.Images
import Site.Css
import Site.Posts
import Site.Blog
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
  | "sass" ==           fileName = True
  | otherwise                    = False
  where
    fileName = takeFileName path

main :: IO ()
main =
  --HL.compile HL.deployDirectory HL.pages
  --putStrLn =<< readProcess "npm" ["run", "css-build"] ""
  hakyllWith defaultConfiguration {ignoreFile = myIgnoreFile} $ do
    images
    css
    posts
    blog
    index

    --match (fromList ["about.rst", "contact.markdown"]) $ do
    --    route   $ setExtension "html"
    --    compile $ pandocCompiler
    --        >>= loadAndApplyTemplate "templates/default.html" defaultContext
    --        >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler
    match "_html/templates/*" $ compile templateBodyCompiler

{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll

import System.FilePath (takeFileName)
import System.Process
import Data.List (isPrefixOf, isSuffixOf)

import Site.Images
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
  | "src" ==            fileName = True
  | "app" ==            fileName = True
  | otherwise                    = False
  where
    fileName = takeFileName path

main :: IO ()
main =
  --HL.compile HL.deployDirectory HL.pages
  --putStrLn =<< readProcess "npm" ["run", "css-build"] ""
  hakyllWith defaultConfiguration {ignoreFile = myIgnoreFile} $ do
    images
    posts
    blog
    index

    match "templates/*" $ compile templateBodyCompiler
    match "_html/templates/*" $ compile templateBodyCompiler

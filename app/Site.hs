{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import Site.Images
import Site.Css
import Site.Posts
import Site.Archive
import Site.Index
import Site.Lib

main :: IO ()
main = hakyll $ do
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

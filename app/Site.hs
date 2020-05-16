{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll

import Site.Images
import Site.Css
import Site.Posts
import Site.Archive
import Site.Index
import Site.Lib
import qualified Html.Lib as HL

main :: IO ()
main = do
  HL.compile HL.deployDirectory [HL.indexPage]
  hakyll $ do
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

module Site.Posts
  ( posts
  ) where

import Data.Monoid (mappend)
import Hakyll

import Site.Lib

posts :: Rules ()
posts = match postPattern $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate postTemplate postCtx
        >>= loadAndApplyTemplate articleTemplate postCtx
        >>= relativizeUrls


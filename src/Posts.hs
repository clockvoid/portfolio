module Posts
  ( posts
  ) where

import Data.Monoid (mappend)
import Hakyll
import Lib

posts :: Rules ()
posts = match postPattern $ do
    route $ setExtension ""
    compile $ pandocCompiler
        >>= loadAndApplyTemplate postTemplate postCtx
        >>= loadAndApplyTemplate defaultTemplate postCtx
        >>= relativizeUrls


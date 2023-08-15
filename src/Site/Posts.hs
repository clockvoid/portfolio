module Site.Posts
  ( posts,
  )
where

import Data.Monoid (mappend)
import Hakyll
import Site.Lib
import Site.Pandoc (myPandocCompiler)

posts :: Rules ()
posts = match postPattern $ do
  route $ setExtension "html"
  compile $
    myPandocCompiler
      >>= loadAndApplyTemplate postTemplate postCtx
      >>= loadAndApplyTemplate articleTemplate postCtx
      >>= relativizeUrls

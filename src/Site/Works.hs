module Site.Works
  ( works,
  )
where

import Data.Monoid (mappend)
import Hakyll
import Site.Lib
import Site.Pandoc (myPandocCompiler)

worksMd :: Pattern
worksMd = fromRegex "static/works.md"

works :: Rules ()
works = match worksMd $ do
  route $ constRoute "works.html"
  let ctx =
        constField "github-link" "static/works.md"
          `mappend` postCtx

  compile $
    myPandocCompiler
      >>= loadAndApplyTemplate postTemplate ctx
      >>= loadAndApplyTemplate articleTemplate ctx
      >>= relativizeUrls

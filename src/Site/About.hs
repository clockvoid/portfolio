module Site.About
  ( about,
  )
where

import Data.Monoid (mappend)
import Hakyll
import Site.Lib
import Site.Pandoc (myPandocCompiler)

aboutMd :: Pattern
aboutMd = fromRegex "static/about.md"

about :: Rules ()
about = match aboutMd $ do
  route $ constRoute "about.html"
  let ctx =
        constField "github-link" "static/about.md"
          `mappend` postCtx

  compile $
    myPandocCompiler
      >>= loadAndApplyTemplate postTemplate ctx
      >>= loadAndApplyTemplate articleTemplate ctx
      >>= relativizeUrls

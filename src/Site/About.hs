module Site.About
  ( about
  ) where

import Data.Monoid (mappend)
import Hakyll

import Site.Lib
import Site.Pandoc (myPandocCompiler)

aboutMd :: Pattern
aboutMd = fromRegex "static/about.md"

about :: Rules ()
about = match aboutMd $ do
    route $ constRoute "about.html"
    compile $ myPandocCompiler
        >>= loadAndApplyTemplate postTemplate postCtx
        >>= loadAndApplyTemplate articleTemplate postCtx
        >>= relativizeUrls


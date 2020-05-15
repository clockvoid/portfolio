module Site.Css
  ( css
  ) where

import Data.Monoid (mappend)
import Hakyll

pattern :: Pattern
pattern = fromRegex "css/*"

css :: Rules ()
css = match pattern $ do
    route   idRoute
    compile compressCssCompiler


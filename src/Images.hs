module Images
  ( images
  ) where

import Data.Monoid (mappend)
import Hakyll

pattern :: Pattern
pattern = fromRegex "images/*"

images :: Rules ()
images = match pattern $ do
    route   idRoute
    compile copyFileCompiler


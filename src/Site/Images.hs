module Site.Images
  ( images,
  )
where

import Data.Monoid (mappend)
import Hakyll

pattern :: Pattern
pattern = fromRegex "favicon.ico"

images :: Rules ()
images = match pattern $ do
  route idRoute
  compile copyFileCompiler

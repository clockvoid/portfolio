{-|
  Copyright Laurent P. René de Cotret (c) 
  This code is made from https://github.com/LaurentRDC/personal-website.
|-}

{-# LANGUAGE OverloadedStrings #-}

module Site.Pandoc
  ( myPandocCompiler 
  ) where

import Text.Pandoc.Definition (Attr, Block (..), Inline (..), Pandoc)
import Text.Pandoc.Walk       (walk)

import Hakyll

import Data.Text

toBulmaHeading :: Block -> Block
toBulmaHeading (Header level attrs xs) = Header (level + 1) newAttrs xs
    where
        (identifier, classes, keyvals) = attrs
        newAttrs = (identifier, classes <> ["title", "is-" <> pack (show $ level + 1)], keyvals)
toBulmaHeading x = x

toBulmaImage :: Inline -> Inline
toBulmaImage (Image attrs xs target) = Image newAttrs xs target
    where
        (identifier, classes, keyvals) = attrs
        newAttrs = (identifier, classes <> ["image"], keyvals)
toBulmaImage x = x

-- ! Transform (or filter) to format heading to Bulma's heading classes.
-- Markdown: ## Title
-- HTML    : <h2 class="title is-2">Title</h2>
bulmaHeadingTransform :: Pandoc -> Pandoc
bulmaHeadingTransform = walk toBulmaHeading

-- Take images and add the bulma "image" class to it
-- Markdown : ![](images/wtv.jpg)
-- Html     : <img src="images/wtv.jpg" class = "image"/>
bulmaImagesTransform :: Pandoc -> Pandoc
bulmaImagesTransform = walk toBulmaImage

-- Combination of all transforms
bulmaTransform :: Pandoc -> Pandoc
bulmaTransform = bulmaHeadingTransform . bulmaImagesTransform

-- | Allow math display, code highlighting, table-of-content, and Pandoc filters
-- Note that the Bulma pandoc filter is always applied last
myPandocCompiler :: Compiler (Item String)
myPandocCompiler = pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions bulmaTransform


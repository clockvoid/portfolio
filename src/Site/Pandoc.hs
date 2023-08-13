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
import Text.Pandoc.Options

import Hakyll

import Data.Text

toBulmaHeading :: Block -> Block
toBulmaHeading (Header level attrs xs) = Header (level + 2) newAttrs xs
    where
        (identifier, classes, keyvals) = attrs
        newAttrs = (identifier, classes <> ["title", "is-" <> pack (show $ level + 3), "bd-anchor-title"], keyvals)
toBulmaHeading x = x

toBulmaImage :: Inline -> Inline
toBulmaImage (Image attrs xs target) = Image newAttrs xs target
    where
        (identifier, classes, keyvals) = attrs
        newAttrs = (identifier, classes <> ["image", "is-in-article"], keyvals)
toBulmaImage x = x

toBulmaTable :: Block -> Block
toBulmaTable (Table a b c d e f) = Div ("", [pack "table is-striped is-bordered"], []) [Table a b c d e f]
toBulmaTable x = x

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

-- Take Table and add to bulma "table" class to it's parent div tag
-- Markdown : | --- | --- |
-- Html     : <div class="table><table>...</table></div>
bulmaTablesTransform :: Pandoc -> Pandoc
bulmaTablesTransform = walk toBulmaTable

-- Combination of all transforms
bulmaTransform :: Pandoc -> Pandoc
bulmaTransform = bulmaHeadingTransform . bulmaImagesTransform . bulmaTablesTransform

-- | Allow math display, code highlighting, table-of-content, and Pandoc filters
-- Note that the Bulma pandoc filter is always applied last
myPandocCompiler :: Compiler (Item String)
myPandocCompiler = pandocCompilerWithTransform defaultHakyllReaderOptions (defaultHakyllWriterOptions { writerHTMLMathMethod = MathJax "" }) bulmaTransform

